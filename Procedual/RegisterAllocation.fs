module RegisterAllocation

type Allocation = Map<Temporary.Temporary,int>
type Spill = Temporary.Temporary list

type Result =
    Success of Allocation
    | Failure of Spill

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
        Failure(spill)

let rec allocateRegisters (frame: Frame.Frame) (insts: InstructionChoice.InstructionWithComment list) =
    let cfg = Liveness.FlowGraph.makeGraph insts
    let igraph = Liveness.Intereference.analyzeIntereference' cfg
    
    let precolored = 
        List.zip Frame.registers (List.init 8 id)
        |> Map.ofList

    match tryAllocateRegisters igraph precolored with
    | Failure(spills) ->
        for t in spills do
            printf "%s " (frame.prettyPrintTemporary t)
        printfn ""
        printfn "regs: %A" Frame.registers
        // do something for spills
        let spills = Set.ofList spills
        let accesses = List.map (fun t -> t,frame.AllocLocal 1 true) (Set.toList spills) |> Map.ofList
        let rec transform (insts: InstructionChoice.InstructionWithComment list) =
            match insts with
            | [] -> []
            | inst :: rest ->
                let useSpills = 
                    Set.intersect spills inst.uses
                    |> Set.toList
                    |> List.map (fun t -> t,Temporary.newTemporary())
                    |> Map.ofList
                let defSpills = 
                    Set.intersect spills inst.definitions
                    |> Set.toList
                    |> List.map (fun t -> t,Temporary.newTemporary())
                    |> Map.ofList
                let loadInsts =
                    let folder insts orig temp =
                        let stmt = IR.Move(IR.Temp(temp),frame.AccessVar (Map.find orig accesses))
                        let inst = (new InstructionChoice.Emitter(frame)).EmitStmt stmt
                        List.append insts inst
                    useSpills
                    |> Map.fold folder []
                let storeInsts =
                    let folder insts orig temp =
                        let stmt = IR.Move(frame.AccessVar (Map.find orig accesses),IR.Temp(temp))
                        let inst = (new InstructionChoice.Emitter(frame)).EmitStmt stmt
                        List.append insts inst
                    defSpills
                    |> Map.fold folder []

                List.concat
                    [
                        loadInsts;
                        [
                            inst
                            |> Map.foldBack (fun orig temp inst -> inst.ReplaceUse orig temp) useSpills
                            |> Map.foldBack (fun orig temp inst -> inst.ReplaceDef orig temp) defSpills
                        ];
                        storeInsts;
                        transform rest
                    ]
        let insts = transform insts
        allocateRegisters frame insts
    | Success(alloc) -> 
        printfn "FP = %A" frame.framePointer
        let alloc = Map.fold (fun alloc t x -> Map.add t x alloc) alloc precolored
        let resolver t =
            match List.tryFindIndex ((=) t) Frame.registers with
            | Some(i) -> sprintf "r%d as %A" i t
            | None -> 
                match Map.tryFind t alloc with
                | Some(i) -> sprintf "r%d as %A" i t
                | None -> 
                    sprintf "<404> as %A" t
        insts,alloc  
        //let resolver t = sprintf "%A" t 
        (*insts
        |> List.map (fun inst -> inst.EmitRealAssembly resolver)*)