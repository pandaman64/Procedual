module Liveness

module IC = InstructionChoice

module DirectedGraph =
    type Node<'a> = {
        id: int;
        value: 'a;
        graph: Graph;
        successors: Map<int,Node<'a>> ref;
        predecessors: Map<int,Node<'a>> ref;
    }
    and Graph() =
        let mutable count = 0

        member this.MakeNode<'a> (v: 'a) : Node<'a> =
            let ret = {
                id = count;
                value = v;
                graph = this;
                successors = ref Map.empty;
                predecessors = ref Map.empty;
            }
            count <- count + 1
            ret

        member this.InitVisited =
            List.init count (fun _ -> ref false)

    let MakeEdge<'a> (from: Node<'a>) (to_: Node<'a>) : unit =
        from.successors := Map.add to_.id to_ !from.successors
        to_.predecessors := Map.add from.id from !to_.predecessors

    let RemoveEdge<'a> (from: Node<'a>) (to_: Node<'a>) : unit =
        from.successors := Map.remove to_.id !from.successors
        to_.predecessors := Map.remove from.id !from.successors

module UndirectedGraph =
    type Node<'a> = {
        id: int;
        value: 'a;
        graph: Graph;
        adjacents: Map<int,Node<'a>> ref;
    }
    and Graph() =
        let mutable count = 0

        member this.MakeNode<'a> (v: 'a) : Node<'a> =
            let ret = {
                id = count;
                value = v;
                graph = this;
                adjacents = ref Map.empty;
            }
            count <- count + 1
            ret

        member this.InitVisited =
            List.init count (fun _ -> ref false)

    let MakeEdge<'a> (from: Node<'a>) (to_: Node<'a>) : unit =
        from.adjacents := Map.add to_.id to_ !from.adjacents
        to_.adjacents := Map.add from.id from !to_.adjacents

    let RemoveEdge<'a> (from: Node<'a>) (to_: Node<'a>) : unit =
        from.adjacents := Map.remove to_.id !from.adjacents
        to_.adjacents := Map.remove from.id !to_.adjacents

    let RemoveNode<'a> (node: Node<'a>) : unit =
        for adj in !node.adjacents do
            RemoveEdge node adj.Value

module FlowGraph =
    [<StructuredFormatDisplayAttribute("{AsString}")>]
    type Node = {
        inst: IC.Instruction;
        inVariables: Set<Temporary.Temporary> ref;
        outVariables: Set<Temporary.Temporary> ref;
    }
    with
        member this.definitions = this.inst.definitions
        member this.uses = this.inst.uses
        member this.AsString =
            let inVariables = 
                !this.inVariables
                |> Set.toList
                |> List.map (sprintf "%A")
                |> String.concat "\\n"
            let outVariables = 
                !this.outVariables
                |> Set.toList
                |> List.map (sprintf "%A")
                |> String.concat "\\n"
            sprintf "%A\\ninVariables:\\n%s\\noutVariables\\n%s" this.inst inVariables outVariables

    [<StructuredFormatDisplayAttribute("{AsString}")>]
    type Liveness = {
        entry: DirectedGraph.Node<Node>;
        exit: DirectedGraph.Node<Node>;
    }
    with
        member this.AsString =
            let mutable ret = "digraph g{\n"
            let visited = this.entry.graph.InitVisited
            let rec visit (node: DirectedGraph.Node<Node>) =
                ret <- ret + sprintf """%d [label="%A"];""" node.id node.value + "\n"
                visited.Item node.id := true
                for s in !node.successors do
                    let s = s.Value
                    ret <- ret + sprintf "%d -> %d;\n" node.id s.id
                    if not !(visited.Item s.id) then
                        visit s
            visit this.entry
            ret <- ret + "}\n"
            ret

    let makeGraph (instructions: IC.Instruction list) : Liveness =
        let rec splitLast xs =
                match xs with
                | [x] -> x,[]
                | x :: xs -> 
                    let last,rest = splitLast xs
                    last,x :: rest
                | [] -> failwith "no last item"
        let split xs =
            match xs with
            | x :: xs ->
                let last,middle = splitLast xs
                x,middle,last
            | [] -> failwith "no items"

        let graph = new DirectedGraph.Graph()
        let nodes = 
            instructions
            |> List.map (fun inst -> { inst = inst; inVariables = ref Set.empty; outVariables = ref Set.empty })
            |> List.map graph.MakeNode 
        let jumpTo = 
            nodes
            |> List.fold 
                (fun j n -> 
                    match n.value.inst with
                    | IC.Label(l) -> Map.add l n j
                    | _ -> j)
                Map.empty

        for (f,t) in List.pairwise nodes do
            DirectedGraph.MakeEdge f t
            match f.value.inst with
            | IC.Operation(op) ->
                match op.jump with
                | None -> ignore "do nothing"
                | Some(labels) -> 
                    List.fold (fun () l -> DirectedGraph.MakeEdge f (Map.find l jumpTo)) () labels
            | _ -> ignore "do nothing"

        let entry,middle,exit = split nodes
        let sorted =
            let visited = graph.InitVisited
            let rec visitEach (ps: DirectedGraph.Node<_> list) = 
                match ps with
                | [] -> []
                | p :: ps when !(visited.Item p.id) -> visitEach ps
                | p :: ps ->
                    // mark early to reduce unnecessary visit
                    let chain = visit p
                    List.append chain (visitEach ps)
            and visit (node: DirectedGraph.Node<_>) =
                visited.Item node.id := true
                node :: visitEach (!node.predecessors |> Map.toList |> List.map snd)
            visit exit

        let solveEquation() =
            let mutable changed = false
            for node in sorted do
                let inVariables = !node.value.inVariables
                let outVariables = !node.value.outVariables

                node.value.inVariables := Set.union node.value.uses (!node.value.outVariables - node.value.definitions)
                node.value.outVariables := 
                    !node.successors
                    |> Map.toList
                    |> List.map (fun (_,s) -> !s.value.inVariables)
                    |> Set.unionMany

                if inVariables <> !node.value.inVariables || outVariables <> !node.value.outVariables
                then
                    changed <- true
            changed

        while solveEquation() do
            ignore "solving"

        { entry = entry; exit = exit }

module Intereference =
    type Node = UndirectedGraph.Node<Temporary.Temporary>
    type Nodes = Node list

    let analyzeIntereference' (liveness: FlowGraph.Liveness) : Nodes =
        let igraph = new UndirectedGraph.Graph()

        let mutable nodes = Map.empty
        let getNode node =
            match nodes.TryFind node with
            | None ->
                let ret = igraph.MakeNode node
                nodes <- Map.add node ret nodes
                ret
            | Some(node) -> node
        let mark lhs rhs =
            if lhs <> rhs
            then
                UndirectedGraph.MakeEdge (getNode lhs) (getNode rhs)
        let visited = liveness.entry.graph.InitVisited
        let rec visit (node: DirectedGraph.Node<FlowGraph.Node>) =
            let visited = visited.Item node.id
            if not !visited
            then
                visited := true
                match node.value.inst with
                | InstructionChoice.Move(dst,src) -> 
                    // for each live-out variables except src, 
                    // add intereference edge to dst
                    for liveOut in !node.value.outVariables do
                        if liveOut <> src
                        then
                            mark dst liveOut
                | _ ->
                    for def in node.value.definitions do
                        for liveOut in !node.value.outVariables do
                            mark def liveOut

                for s in !node.successors do
                    visit s.Value

        visit liveness.entry

        nodes
        |> Map.toList
        |> List.map snd