module InstructionChoice

open Common

type OpCode =
    BINOP of Temporary.Temporary * Op * Temporary.Temporary
    | LOAD of Temporary.Temporary * Temporary.Temporary
    | STORE of Temporary.Temporary * Temporary.Temporary
    | ADDI of Temporary.Temporary * int
    | LDI of Temporary.Temporary * int
    | JUMP of Temporary.Label
    | JR of Temporary.Temporary
    | JAL of Temporary.Label
    | JALR of Temporary.Temporary
    | BNZ of Temporary.Temporary * Temporary.Label
    | NOP
with
    member this.Replace from to_ =
        let replace x = 
            if x = from then to_ else x
        match this with
        | BINOP(lhs,op,rhs) -> BINOP(replace lhs,op,replace rhs)
        | LOAD(dst,src) -> LOAD(replace dst,replace src)
        | STORE(dst,src) -> STORE(replace dst,replace src)
        | ADDI(dst,x) -> ADDI(replace dst,x)
        | LDI(dst,x) -> LDI(replace dst,x)
        | JR(t) -> JR(replace t)
        | JALR(t) -> JALR(replace t)
        | BNZ(t,l) -> BNZ(replace t,l)
        | JUMP(_)         
        | JAL(_)
        | NOP -> this
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
                op = op.op.Replace from to_;
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
                op = op.op.Replace from to_;
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
            | ADDI(dst,x) -> sprintf "ADDI %s,%d" (resolver dst) x
            | LDI(dst,x) -> sprintf "LDI %s,%d" (resolver dst) x
            | JUMP(l) -> sprintf "J %A" l
            | JR(t) -> sprintf "JR %s" (resolver t)
            | JAL(l) -> sprintf "JAL %A" l
            | JALR(t) -> sprintf "JALR %s" (resolver t)
            | BNZ(t,l) -> sprintf "BNZ %s,%A" (resolver t) l
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

[<StructuredFormatDisplayAttribute("{AsString}")>]
type InstructionWithComment = { inst: Instruction; comment: string }
with
    member this.definitions =
        this.inst.definitions
    member this.uses =
        this.inst.uses
    member this.ReplaceUse from to_ =
        { inst = this.inst.ReplaceUse from to_; comment = this.comment }
    member this.ReplaceDef from to_ =
        { inst = this.inst.ReplaceDef from to_; comment = this.comment }
    member this.EmitRealAssembly resolver debug =
        if debug
        then
            sprintf "%s //%s" (this.inst.EmitRealAssembly resolver) this.comment
        else
            this.inst.EmitRealAssembly resolver
    member this.AsString =
        sprintf "%A //%s" this.inst this.comment 

type Emitter(frame: Frame.Frame) =
    // save instructions in REVERSE order
    let mutable instructions = []

    let Emit comment inst =
        instructions <- { inst = inst; comment = comment } :: instructions

    let rec choiceExpr (expr: IR.Expr) : Temporary.Temporary =
        match expr with
        | IR.BinaryOp(lhs,Common.Add,IR.Const(0)) ->
            choiceExpr lhs
        | IR.BinaryOp(lhs,Common.Add,IR.Const(x)) ->
            let lhs = choiceExpr lhs
            let t = Temporary.newTemporary()
            Emit "MV beore ADDI" (Move(t,lhs))
            {
                op = ADDI(t,x);
                dst = [t];
                src = [t];
                jump = None
            }
            |> Operation
            |> Emit "ADDI"
            t
        | IR.BinaryOp(IR.Const(0),Common.Add,rhs) ->
            choiceExpr rhs
        | IR.BinaryOp(IR.Const(x),Common.Add,rhs) ->
            let rhs = choiceExpr rhs
            let t = Temporary.newTemporary()
            Emit "MOVE before ADDI" (Move(t,rhs))
            {
                op = ADDI(t,x);
                dst = [t];
                src = [t];
                jump = None
            }
            |> Operation
            |> Emit "ADDI"
            t
        | IR.BinaryOp(lhs,Common.Subtract,IR.Const(x)) ->
            let lhs = choiceExpr lhs
            let t = Temporary.newTemporary()
            Emit "MOVE before SUBI" (Move(t,lhs))
            {
                op = ADDI(t,-x);
                dst = [t];
                src = [t];
                jump = None
            }
            |> Operation
            |> Emit "SUBI"
            t
        | IR.BinaryOp(lhs,op,rhs) ->
            let lhs = choiceExpr lhs
            let rhs = choiceExpr rhs
            let t = Temporary.newTemporary()
            Emit "MOVE before BinOp" (Move(t,lhs))
            {
                op = BINOP(t,op,rhs);
                dst = [t];
                src = [t; rhs];
                jump = None;
            }
            |> Operation
            |> Emit "BinOp"
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
            |> Emit "Call via label directly"
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
            |> Emit "Call by function value"
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
            |> Emit "Load Constant"
            t
        | IR.Temp(t) when t = frame.framePointer ->
            Emit "Load Stack Pointer" (Move(t,Frame.stackPointer))
            {
                op = ADDI(t,-frame.frameSize);
                dst = [t];
                src = [t];
                jump = None;
            }
            |> Operation
            |> Emit "Load Frame Pointer (by subtracting frame size from stack pointer)"
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
            |> Emit "Load from memory"
            t
    and choiceArgs (index: int) (exprs: IR.Expr list) : Temporary.Temporary list =
        match exprs with
        | [] -> []
        | expr :: exprs ->
            let t = choiceExpr expr
            // スピルする際にはここで命令発行すればいいね
            Emit (sprintf "assigning %d'th argument" index) (Move(Frame.registers.Item index,t))
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
            |> Emit "Calling via label (stmt)"
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
            |> Emit "Call by function value (stmt)"
        | IR.ExprStmt(expr) -> 
            choiceExpr expr
            |> ignore
        | IR.MarkLabel(l) -> Label(l) |> Emit "marking label"
        | IR.Jump(IR.LabelRef(l),labels) when labels = [l] ->
            {
                op = JUMP(l);
                dst = [];
                src = [];
                jump = Some([l]);
            }
            |> Operation
            |> Emit "Jump Immediate"
        | IR.Jump(l,labels) ->
            let l = choiceExpr l
            {
                op = JR(l);
                dst = [];
                src = [l];
                jump = Some(labels)
            }
            |> Operation
            |> Emit "Jump by register value"
        | IR.ConditionalJump(cond,t,f) ->
            // assume program falls through else-clause
            let cond = choiceExpr cond
            {
                op = BNZ(cond,t);
                dst = [];
                src = [cond];
                jump = Some([t])
            }
            |> Operation
            |> Emit "Conditional Jump (else-clause must be down here)"
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
            |> Emit "Loading src value to temporary"
            let dst = choiceExpr dst
            {
                op = STORE(dst,t);
                dst = [];
                src = [dst; t];
                jump = None;
            }
            |> Operation
            |> Emit "Assigning temporary value to dst"
        | IR.Move(IR.Mem(IR.Temp(t)),value) ->
            let value = choiceExpr value
            {
                op = STORE(t,value);
                dst = [];
                src = [t; value];
                jump = None
            }
            |> Operation
            |> Emit "Assigning value to memory (via temporary containing the address)"
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
            |> Emit "Assigning value to memory (by calculating the address)"
        | IR.Move(dst,IR.Mem(IR.Temp(src))) ->
            let dst = choiceExpr dst
            {
                op = LOAD(dst,src);
                dst = [dst];
                src = [src];
                jump = None;
            }
            |> Operation
            |> Emit "Loading value from memory (via temporary containing the address)"
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
            |> Emit "Loading value from memory (by calculating address)"
        | IR.Move(IR.Temp(t),e) ->
            let e = choiceExpr e
            Emit "Moving value to temporary" (Move(t,e))
        | IR.Move(_,_) -> failwith "destination of move must be memory"

    member this.EmitStmt stmt : InstructionWithComment list =
        if List.isEmpty instructions
        then
            choiceStmt stmt
        List.rev instructions

    member this.EmitDecl (decl: TypeCheck.Declaration) : InstructionWithComment list =
        if List.isEmpty instructions
        then
            decl.body
            |> List.map (fun block -> for stmt in block do choiceStmt stmt)
            |> ignore

            // mark special registers used after exit
            instructions <- 
                {
                    inst = Operation{
                        op = NOP;
                        dst = [];
                        src = 
                            [
                                // Return Address (r7)
                                Frame.returnAddress;
                                // Stack Pointer (r6)
                                Frame.stackPointer;
                                // Return Value (r0)
                                Frame.returnValue;
                                // Callee Save Register (r4, r5)
                                List.item 4 Frame.registers;
                                List.item 5 Frame.registers;
                            ];
                        jump = Some([]);
                    };
                    comment = "Saving RA, SP, RV, R4, R5"
                }
                :: instructions
        List.rev instructions

let choiceInstructions (decls: TypeCheck.Declaration list) : Map<Name,InstructionWithComment list> =
    decls
    |> List.map (fun decl -> decl.name,(new Emitter(decl.frame)).EmitDecl decl) 
    |> Map.ofList