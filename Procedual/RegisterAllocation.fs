module RegisterAllocation

type Allocation = Map<Temporary.Temporary,int>
type Spill = Temporary.Temporary list

type Result =
    Success of Allocation
    | Fail of Spill

let tryAllocateRegisters (nodes: Liveness.Intereference.Nodes) (precolored: Allocation) =
    let comp (k: int) (n1: Liveness.Intereference.Node) (n2: Liveness.Intereference.Node) =
        let k1 = n1.adjacents.Value.Count
        let k2 = n2.adjacents.Value.Count
        if k1 < k && k2 < k
        then
            k2 - k1
        elif k1 >= k && k2 >= k
        then
            k1 - k2
        elif k1 < k && k2 >= k
        then
            -1
        elif k1 >= k && k2 < k
        then
            1
        else
            failwith "unreacheable"
    
    let K = 8

    let sorted = List.sortWith (comp K) nodes
            
    let stack = List.rev sorted |> List.where (fun n -> not (precolored.ContainsKey n.value))
    let mutable colors = precolored
    let mutable spill = []

    for node in stack do
        let mutable ks = List.init 8 id |> Set.ofList
        for adj in !node.adjacents do
            match colors.TryFind adj.Value.value with
            | None -> ignore "do nothing"
            | Some(k) -> ks <- ks.Remove k

        if ks.IsEmpty
        then
            spill <- node.value :: spill
            //colors <- Map.add node.value -1 colors
        else
            colors <- Map.add node.value ks.MinimumElement colors

    if spill.IsEmpty
    then
        Success(colors)
    else
        Fail(spill)

let rec allocateRegisters (insts: InstructionChoice.Instruction list) : Allocation =
    failwith "?"