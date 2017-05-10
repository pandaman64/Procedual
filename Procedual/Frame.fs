module Frame

open Common

let stackPointer = Temporary.newTemporary()
let returnValue = Temporary.newTemporary()
let returnAddress = Temporary.newTemporary()

let registers =
    [
        // r0 = RV
        returnValue;
        // r1
        Temporary.newTemporary();
        // r2
        Temporary.newTemporary();
        // r3
        Temporary.newTemporary();
        // r4
        Temporary.newTemporary();
        // r5
        Temporary.newTemporary();
        // r6 = SP
        stackPointer;
        // r7 = RA
        returnAddress;
    ]
// caller-save registers are r0(RV), r1, r2, r3, r6(SP), r7(RA)
// callee-save registers are r4, r5
let calldefs = 
    [ 0; 1; 2; 3; 6; 7; ]
    |> List.map (fun i -> registers.Item i)

type Access = 
    InMemory of int // offset from fp
    | InRegister of Temporary.Temporary // temporary register
    | Literal of IR.Expr
type Frame(name,arguments : (int * bool) list,framePointer) =
    let mutable currentOffset: int = 0

    member this.arguments =
        let mapFolder offset (size,spill) =
            let offset = offset - size
            if spill
            then
                InMemory(offset)
            else
                InRegister(Temporary.newTemporary())
            ,offset
        List.mapFold mapFolder 0 arguments
        |> fst

    member this.Name : Var = name

    member this.AccessVar access =
        match access with
        | InMemory(offset) -> IR.Mem(IR.BinaryOp(IR.Temp(framePointer),Add,IR.Const(offset)))
        | InRegister(reg) -> IR.Mem(IR.Temp(reg))
        | Literal(e) -> e

    member this.AllocLocal (size: int) (spill: bool) =
        if spill
        then
            let offset = currentOffset
            currentOffset <- currentOffset + size
            InMemory(offset)
        else
            InRegister(Temporary.newTemporary())
