module Liveness

module IC = InstructionChoice

// Control Flow Graph
module Graph =
    type Node<'a> = {
        id: int;
        value: 'a;
        graph: Graph;
        successors: Node<'a> list ref;
        predecessors: Node<'a> list ref;
    }
    and Graph() =
        let mutable count = 0

        member this.MakeNode<'a> (v: 'a) : Node<'a> =
            let ret = {
                id = count;
                value = v;
                graph = this;
                successors = ref [];
                predecessors = ref [];
            }
            count <- count + 1
            ret

        member this.InitVisited =
            List.init count (fun _ -> ref false)

    let MakeEdge<'a> (from: Node<'a>) (to_: Node<'a>) : unit =
        from.successors := to_ :: !from.successors
        to_.predecessors := from :: !to_.predecessors

    let RemoveEdge<'a when 'a : equality> (from: Node<'a>) (to_: Node<'a>) : unit =
        let isDifferentInstance a b = LanguagePrimitives.PhysicalEquality a b |> not
        from.successors := List.where (isDifferentInstance to_) !from.successors
        to_.predecessors := List.where (isDifferentInstance from) !to_.predecessors

module FlowGraph =
    [<StructuredFormatDisplayAttribute("{AsString}")>]
    type Node = {
        inst: IC.Instruction;
        inVariables: Set<Temporary.Temporary> ref;
        outVariables: Set<Temporary.Temporary> ref;
    }
    with
        member this.definitions =
            match this.inst with
            | IC.Operation(op) -> op.dst
            | IC.Move(dst,_) -> [dst]
            | IC.Label(_) -> []
            |> Set.ofList
        member this.uses =
            match this.inst with
            | IC.Operation(op) -> op.src
            | IC.Move(_,src) -> [src]
            | IC.Label(_) -> []
            |> Set.ofList
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
        entry: Graph.Node<Node>;
        exit: Graph.Node<Node>;
    }
    with
        member this.AsString =
            let mutable ret = "digraph g{\n"
            let visited = this.entry.graph.InitVisited
            let rec visit (node: Graph.Node<Node>) =
                ret <- ret + sprintf """%d [label="%A"];""" node.id node.value + "\n"
                visited.Item node.id := true
                for s in !node.successors do
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

        let graph = new Graph.Graph()
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
            Graph.MakeEdge f t
            match f.value.inst with
            | IC.Operation(op) ->
                match op.jump with
                | None -> ignore "do nothing"
                | Some(labels) -> 
                    List.fold (fun () l -> Graph.MakeEdge f (Map.find l jumpTo)) () labels
            | _ -> ignore "do nothing"

        let entry,middle,exit = split nodes
        let sorted =
            let visited = graph.InitVisited
            let rec visitEach (ps: Graph.Node<_> list) = 
                match ps with
                | [] -> []
                | p :: ps when !(visited.Item p.id) -> visitEach ps
                | p :: ps ->
                    // mark early to reduce unnecessary visit
                    let chain = visit p
                    List.append chain (visitEach ps)
            and visit (node: Graph.Node<_>) =
                visited.Item node.id := true
                node :: visitEach (!node.predecessors)
            visit exit

        let solveEquation() =
            let mutable changed = false
            for node in sorted do
                let inVariables = !node.value.inVariables
                let outVariables = !node.value.outVariables

                node.value.inVariables := Set.union node.value.uses (!node.value.outVariables - node.value.definitions)
                node.value.outVariables := 
                    !node.successors
                    |> List.map (fun s -> !s.value.inVariables)
                    |> Set.unionMany

                if inVariables <> !node.value.inVariables || outVariables <> !node.value.outVariables
                then
                    changed <- true
            changed

        while solveEquation() do
            ignore "solving"

        { entry = entry; exit = exit }