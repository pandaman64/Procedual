module InstructionChoice

open Common

type OpCode =
    BINOP of Op
    | CALL
    | LOAD
    | STORE
    | ADDI of int
    | LDI of int
    | LDLABEL of Temporary.Label
    | NOP
    | JUMP
    | BEQ

type Operation = {
    op: OpCode;
    dst: Temporary.Temporary list
    src: Temporary.Temporary list
    jump: Temporary.Label list option
}

type Instruction =
    Operation of Operation
    | Label of Temporary.Label
    | Move of Temporary.Temporary * Temporary.Temporary // dst * src

type Emitter(decl: TypeCheck.Declaration) =
    let mutable instructions = []

    let Emit inst =
        instructions <- inst :: instructions

    let rec choiceExpr (expr: IR.Expr) : Temporary.Temporary =
        match expr with
        | IR.BinaryOp(lhs,Common.Add,IR.Const(x)) ->
            let lhs = choiceExpr lhs
            let t = Temporary.newTemporary()
            Emit (Move(t,lhs))
            {
                op = ADDI(x);
                dst = [t];
                src = [];
                jump = None
            }
            |> Operation
            |> Emit
            t
        | IR.BinaryOp(IR.Const(x),Common.Add,rhs) ->
            let rhs = choiceExpr rhs
            let t = Temporary.newTemporary()
            Emit (Move(t,rhs))
            {
                op = ADDI(x);
                dst = [t];
                src = [];
                jump = None
            }
            |> Operation
            |> Emit
            t
        | IR.BinaryOp(lhs,Common.Subtract,IR.Const(x)) ->
            let lhs = choiceExpr lhs
            let t = Temporary.newTemporary()
            Emit (Move(t,lhs))
            {
                op = ADDI(-x);
                dst = [t];
                src = [];
                jump = None
            }
            |> Operation
            |> Emit
            t
        | IR.BinaryOp(lhs,op,rhs) ->
            let lhs = choiceExpr lhs
            let rhs = choiceExpr rhs
            let t = Temporary.newTemporary()
            Emit (Move(t,lhs))
            {
                op = BINOP(op);
                dst = [t];
                src = [rhs];
                jump = None;
            }
            |> Operation
            |> Emit
            t
        | IR.Call(f,xs) -> 
            {
                op = CALL;
                dst = Frame.calldefs;
                src = choiceExpr f :: choiceArgs 0 xs;
                jump = None
            }
            |> Operation
            |> Emit
            Frame.returnValue
        | IR.Const(x) ->
            let t = Temporary.newTemporary()
            {
                op = LDI(x);
                dst = [t];
                src = [];
                jump = None;
            }
            |> Operation
            |> Emit
            t
        | IR.Temp(t) -> t
        | IR.ExprSequence(_,_) -> failwith "linearization must remove ESeqs."
        | IR.LabelRef(l) -> 
            let t = Temporary.newTemporary()
            {
                op = LDLABEL(l);
                dst = [t];
                src = [];
                jump = None;
            }
            |> Operation
            |> Emit
            t
        | IR.Mem(e) -> 
            let t = Temporary.newTemporary()
            {
                op = LOAD;
                dst = [t];
                src = [choiceExpr e];
                jump = None;
            }
            |> Operation
            |> Emit
            t
    and choiceArgs (index: int) (exprs: IR.Expr list) : Temporary.Temporary list =
        match exprs with
        | [] -> []
        | expr :: exprs ->
            let t = choiceExpr expr
            // スピルする際にはここで命令発行すればいいね
            Emit (Move(Frame.registers.Item index,t))
            t :: choiceArgs (index + 1) exprs

    let choiceStmt (stmt: IR.Statement) =
        match stmt with
        | IR.Sequence(_,_) -> failwith "linearization must remove Seqs."
        | IR.Nop -> failwith "linearization should remove Nops."
        | IR.ExprStmt(IR.Call(f,xs)) ->
            {
                op = CALL;
                dst = Frame.calldefs;
                src = choiceExpr f :: choiceArgs 0 xs;
                jump = None
            }
            |> Operation
            |> Emit
        | IR.ExprStmt(_) -> failwith "i have no idea"
        | IR.MarkLabel(l) -> Label(l) |> Emit
        | IR.Jump(l,labels) -> 
            {
                op = JUMP;
                dst = [];
                src = [choiceExpr l];
                jump = Some(labels)
            }
            |> Operation
            |> Emit
        | IR.ConditionalJump(cond,t,f) ->
            // assume program falls through else-clause
            {
                op = BEQ;
                dst = [];
                src = [choiceExpr cond];
                jump = Some([t])
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Mem(dst),IR.Mem(src)) ->
            let t = Temporary.newTemporary()
            {
                op = LOAD;
                dst = [t];
                src = [choiceExpr src];
                jump = None;
            }
            |> Operation
            |> Emit
            {
                op = STORE;
                dst = [choiceExpr dst];
                src = [t];
                jump = None;
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Mem(IR.Temp(t)),value) ->
            {
                op = STORE;
                dst = [t];
                src = [choiceExpr value];
                jump = None
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Mem(dst),src) ->
            {
                op = STORE;
                dst = [choiceExpr dst];
                src = [choiceExpr src];
                jump = None
            }
            |> Operation
            |> Emit
        | IR.Move(dst,IR.Mem(IR.Temp(src))) ->
            {
                op = LOAD;
                dst = [choiceExpr dst];
                src = [src];
                jump = None;
            }
            |> Operation
            |> Emit
        | IR.Move(dst,IR.Mem(src)) ->
            {
                op = LOAD;
                dst = [choiceExpr dst];
                src = [choiceExpr src];
                jump = None;
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Temp(t),e) ->
            let e = choiceExpr e
            Emit (Move(t,e))
        | IR.Move(_,_) -> failwith "destination of move must be memory"

    member this.Instructions : Instruction list =
        if List.isEmpty instructions
        then
            decl.body
            |> List.map (fun block -> for stmt in block do choiceStmt stmt)
            |> ignore
            // mark special registers used after exit
            instructions <- 
                Operation{
                    op = NOP;
                    dst = [Frame.returnAddress; Frame.stackPointer; Frame.returnValue];
                    src = [];
                    jump = Some([]);
                }
                :: instructions
        List.rev instructions

let choiceInstructions (decls: TypeCheck.Declaration list) : Map<Name,Instruction list> =
    decls
    |> List.map (fun decl -> decl.name,(new Emitter(decl)).Instructions) 
    |> Map.ofList