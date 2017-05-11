module InstructionChoice

open Common

type OpCode =
    BINOP of Temporary.Temporary * Op * Temporary.Temporary
    | LOAD of Temporary.Temporary * Temporary.Temporary
    | STORE of Temporary.Temporary * Temporary.Temporary
    | SETSP of Frame.Frame
    | ADDI of Temporary.Temporary * int
    | LDI of Temporary.Temporary * int
    | JUMP of Temporary.Label
    | JR of Temporary.Temporary
    | JAL of Temporary.Label
    | JALR of Temporary.Temporary
    | BEQ of Temporary.Temporary * Temporary.Label
    | NOP

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
with
    member this.definitions =
        match this with
        | Operation(op) -> op.dst
        | Move(dst,_) -> [dst]
        | Label(_) -> []
        |> Set.ofList
    member this.uses =
        match this with
        | Operation(op) -> op.src
        | Move(_,src) -> [src]
        | Label(_) -> []
        |> Set.ofList
    member this.ReplaceUse from to_ =
        match this with
        | Operation(op) ->
            {
                op = op.op;
                dst = op.dst;
                src = op.src |> List.map (fun t -> if t = from then to_ else t);
                jump = op.jump;
            }
            |> Operation
        | Move(dst,src) when src = from -> Move(dst,to_)
        | _ -> this
    member this.ReplaceDef from to_ =
        match this with
        | Operation(op) ->
            {
                op = op.op;
                dst = op.dst |> List.map (fun t -> if t = from then to_ else t);
                src = op.src;
                jump = op.jump;
            }
            |> Operation
        | Move(dst,src) when dst = from -> Move(to_,src)
        | _ -> this
    member this.EmitRealAssembly resolver =
        match this with
        | Move(dst,src) -> sprintf "MV %s,%s" (resolver dst) (resolver src)
        | Label(l) -> sprintf "%A:" l
        | Operation(op) ->
            match op.op with
            | LOAD(dst,src) -> sprintf "LD %s,%s" (resolver dst) (resolver src)
            | STORE(dst,src) -> sprintf "ST %s,%s" (resolver dst) (resolver src)
            | SETSP(frame) -> sprintf "ADDI r6,%d" frame.frameSize
            | ADDI(dst,x) -> sprintf "ADDI %s,%d" (resolver dst) x
            | LDI(dst,x) -> sprintf "LDI %s,%d" (resolver dst) x
            | JUMP(l) -> sprintf "J %A" l
            | JR(t) -> sprintf "JR %s" (resolver t)
            | JAL(l) -> sprintf "JAL %A" l
            | JALR(t) -> sprintf "JALR %s" (resolver t)
            | BEQ(t,l) -> sprintf "BEQ %s,%A" (resolver t) l
            | NOP -> sprintf ""
            | BINOP(dst,op,src) ->
                let op =
                    match op with
                    | Add -> "ADD" 
                    | Subtract -> "SUB"
                    | Multiply -> "MUL"
                    | Divide -> "DIV"
                    | Equal -> "EQ"
                    | NotEqual -> "NEQ"
                    | GreaterThan -> "GT"
                    | GreaterThanOrEq -> "GTE"
                    | LessThan -> "LT"
                    | LessThanOrEq -> "LTE"
                    | Assign -> failwith "unreacheable"
                sprintf "%s %s,%s" op (resolver dst) (resolver src)

type Emitter() =
    let mutable instructions = []

    let Emit inst =
        instructions <- inst :: instructions

    let rec choiceExpr (expr: IR.Expr) : Temporary.Temporary =
        match expr with
        | IR.BinaryOp(lhs,Common.Add,IR.Const(0)) ->
            choiceExpr lhs
        | IR.BinaryOp(lhs,Common.Add,IR.Const(x)) ->
            let lhs = choiceExpr lhs
            let t = Temporary.newTemporary()
            Emit (Move(t,lhs))
            {
                op = ADDI(t,x);
                dst = [t];
                src = [t];
                jump = None
            }
            |> Operation
            |> Emit
            t
        | IR.BinaryOp(IR.Const(0),Common.Add,rhs) ->
            choiceExpr rhs
        | IR.BinaryOp(IR.Const(x),Common.Add,rhs) ->
            let rhs = choiceExpr rhs
            let t = Temporary.newTemporary()
            Emit (Move(t,rhs))
            {
                op = ADDI(t,x);
                dst = [t];
                src = [t];
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
                op = ADDI(t,-x);
                dst = [t];
                src = [t];
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
                op = BINOP(t,op,rhs);
                dst = [t];
                src = [t; rhs];
                jump = None;
            }
            |> Operation
            |> Emit
            t
        | IR.Call(IR.LabelRef(l),xs) ->
            let xs = choiceArgs 0 xs
            {
                op = JAL(l);
                dst = Frame.calldefs
                src = xs;
                jump = Some([l]);
            }
            |> Operation
            |> Emit
            Frame.returnValue
        | IR.Call(f,xs) ->
            let f = choiceExpr f
            let xs = choiceArgs 0 xs
            {
                op = JALR(f);
                dst = Frame.calldefs;
                src = f :: xs;
                jump = None
            }
            |> Operation
            |> Emit
            Frame.returnValue
        | IR.Const(x) ->
            let t = Temporary.newTemporary()
            {
                op = LDI(t,x);
                dst = [t];
                src = [];
                jump = None;
            }
            |> Operation
            |> Emit
            t
        | IR.Temp(t) -> t
        | IR.ExprSequence(_,_) -> failwith "linearization must remove ESeqs."
        | IR.LabelRef(l) -> failwith "no label ref can appear"
        | IR.Mem(e) -> 
            let t = Temporary.newTemporary()
            let e = choiceExpr e
            {
                op = LOAD(t,e);
                dst = [t];
                src = [e];
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
        | IR.ExprStmt(IR.Call(IR.LabelRef(l),xs)) ->
            let xs = choiceArgs 0 xs
            {
                op = JAL(l);
                dst = Frame.calldefs;
                src = xs;
                jump = Some([l]);
            }
            |> Operation
            |> Emit
        | IR.ExprStmt(IR.Call(f,xs)) ->
            let f = choiceExpr f
            let xs = choiceArgs 0 xs
            {
                op = JALR(f);
                dst = Frame.calldefs;
                src = f :: xs;
                jump = Some([])
            }
            |> Operation
            |> Emit
        | IR.ExprStmt(_) -> failwith "i have no idea"
        | IR.MarkLabel(l) -> Label(l) |> Emit
        | IR.Jump(IR.LabelRef(l),labels) when labels = [l] ->
            {
                op = JUMP(l);
                dst = [];
                src = [];
                jump = Some([l]);
            }
            |> Operation
            |> Emit
        | IR.Jump(l,labels) ->
            let l = choiceExpr l
            {
                op = JR(l);
                dst = [];
                src = [l];
                jump = Some(labels)
            }
            |> Operation
            |> Emit
        | IR.ConditionalJump(cond,t,f) ->
            // assume program falls through else-clause
            let cond = choiceExpr cond
            {
                op = BEQ(cond,t);
                dst = [];
                src = [cond];
                jump = Some([t])
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Mem(dst),IR.Mem(src)) ->
            let t = Temporary.newTemporary()
            let src = choiceExpr src
            {
                op = LOAD(t,src);
                dst = [t];
                src = [src];
                jump = None;
            }
            |> Operation
            |> Emit
            let dst = choiceExpr dst
            {
                op = STORE(dst,t);
                dst = [];
                src = [dst; t];
                jump = None;
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Mem(IR.Temp(t)),value) ->
            let value = choiceExpr value
            {
                op = STORE(t,value);
                dst = [];
                src = [t; value];
                jump = None
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Mem(dst),src) ->
            let dst = choiceExpr dst
            let src = choiceExpr src
            {
                op = STORE(dst,src);
                dst = [];
                src = [dst; src];
                jump = None
            }
            |> Operation
            |> Emit
        | IR.Move(dst,IR.Mem(IR.Temp(src))) ->
            let dst = choiceExpr dst
            {
                op = LOAD(dst,src);
                dst = [dst];
                src = [src];
                jump = None;
            }
            |> Operation
            |> Emit
        | IR.Move(dst,IR.Mem(src)) ->
            let dst = choiceExpr dst
            let src = choiceExpr src
            {
                op = LOAD(dst,src);
                dst = [dst];
                src = [src];
                jump = None;
            }
            |> Operation
            |> Emit
        | IR.Move(IR.Temp(t),e) ->
            let e = choiceExpr e
            Emit (Move(t,e))
        | IR.Move(_,_) -> failwith "destination of move must be memory"

    member this.EmitStmt stmt : Instruction list =
        if List.isEmpty instructions
        then
            choiceStmt stmt
        List.rev instructions

    member this.InsertStackPointerOperation (frame: Frame.Frame) : unit =
        let entry,body,exit = 
            let entry,body = List.splitAt 1 instructions
            let body,exit = List.splitAt (body.Length - 1) body
            entry,body,exit

        let set =
            [
                Move(frame.framePointer,Frame.stackPointer);
                {
                    op = SETSP(frame);
                    dst = [ Frame.stackPointer ];
                    src = [ Frame.stackPointer ];
                    jump = None;
                }
                |> Operation;
            ]
        let reset =
            [
                Move(Frame.stackPointer,frame.framePointer);
            ]
        instructions <-
            List.concat [
                entry;
                set;
                body;
                reset;
                exit;
            ]

    member this.EmitDecl (decl: TypeCheck.Declaration) : Instruction list =
        if List.isEmpty instructions
        then
            decl.body
            |> List.map (fun block -> for stmt in block do choiceStmt stmt)
            |> ignore

            this.InsertStackPointerOperation decl.frame

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
    |> List.map (fun decl -> decl.name,(new Emitter()).EmitDecl decl) 
    |> Map.ofList