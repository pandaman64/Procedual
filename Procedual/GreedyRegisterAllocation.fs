module GreedyRegisterAllocation

type Allocation = Map<Temporary.Temporary,int>
type Spill = Temporary.Temporary list

let rec tryAllocateRegisters (igraph: Liveness.Intereference.Nodes) (initial: Allocation) : Allocation * Temporary.Temporary list =
    let colors = [ 0; 1; 2; 3; 4; 5; 6; 7; ] |> Set.ofList
    let folder ((allocation,spills): Allocation * Temporary.Temporary list) (node: Liveness.Intereference.Node) =
        match initial.TryFind node.value with
        | Some(_) -> allocation,spills
        | None ->
            let adjacent_colors = 
                !node.adjacents
                |> Map.toList
                |> List.map snd
                |> List.choose (fun adj -> allocation.TryFind adj.value)
                |> Set.ofList
            let available_colors = colors - adjacent_colors
            match Set.toList available_colors with
            | c :: _ -> Map.add node.value c allocation,spills
            | _ -> allocation,node.value :: spills
    List.fold folder (initial,[]) igraph

let spillTemporary (frame: Frame.Frame) (insts: InstructionChoice.InstructionWithComment list) (t: Temporary.Temporary) =
    let access = frame.AllocLocal 1 true
    let rec impl (insts: InstructionChoice.InstructionWithComment list) =
        match insts with
        | [] -> []
        | inst :: insts ->
            let t' = Temporary.newTemporary()
            let useReplace =
                if inst.uses.Contains t
                then
                    IR.Move(IR.Temp(t'),frame.AccessVar access)
                    |> (new InstructionChoice.Emitter(frame)).EmitStmt
                else
                    []
            let defReplace =
                if inst.definitions.Contains t
                then
                    IR.Move(frame.AccessVar access,IR.Temp(t'))
                    |> (new InstructionChoice.Emitter(frame)).EmitStmt
                else
                    []
            [
                useReplace;
                [ (inst.ReplaceUse t t').ReplaceDef t t' ];
                defReplace;
                impl insts
            ]
            |> List.concat
    impl insts

let rec allocateRegisters (frame: Frame.Frame) (insts: InstructionChoice.InstructionWithComment list) =
    let canSpill t =
        if t = frame.framePointer || List.contains t Frame.registers
        then
            false
        else
            true
    let initial =  
        List.indexed Frame.registers
        |> List.map (fun (i,t) -> (t,i))
        |> Map.ofList
    let cfg = Liveness.FlowGraph.makeGraph insts
    let igraph = Liveness.Intereference.analyzeIntereference' cfg
    match tryAllocateRegisters igraph initial with
    | allocation,[] -> insts,allocation
    | _,spills ->
        let spills = List.filter canSpill spills
        match spills with
        | spill :: _ -> allocateRegisters frame (spillTemporary frame insts spill)
        | _ -> failwith "no idea. should spill irrelevant temporaries?"
