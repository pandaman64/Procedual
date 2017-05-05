module IR

open Common

[<StructuredFormatDisplayAttribute("{AsString}")>]
type Expr =
    Const of int
    | LabelRef of Temporary.Label
    | Temp of Temporary.Temporary
    | BinaryOp of Expr * Op * Expr
    | Call of Expr * Expr list
    | ExprSequence of Statement * Expr
    | Mem of Expr
with
    member this.AsString =
        match this with
        | Const(x) -> sprintf "%d" x
        | LabelRef(l) -> sprintf "%A" l
        | Temp(t) -> sprintf "%A" t
        | BinaryOp(lhs,op,rhs) -> sprintf "(%A %A %A)" lhs op rhs
        | Call(f,xs) ->
            let arguments = 
                xs
                |> List.map (sprintf "%A")
                |> String.concat ","
            sprintf "%A(%s)" f arguments
        | ExprSequence(s,e) -> sprintf "%A; %A" s e
        | Mem(e) -> sprintf "(*%A)" e
and 
    [<StructuredFormatDisplayAttribute("{AsString}")>]
    Statement =
        Move of Expr * Expr
        | ExprStmt of Expr
        | Jump of Expr * Temporary.Label list
        | ConditionalJump of Expr * Temporary.Label * Temporary.Label
        | Sequence of Statement * Statement
        | MarkLabel of Temporary.Label
        | Nop
    with
        member this.AsString =
            match this with
            | Move(dst,src) -> sprintf "%A <= %A" dst src
            | ExprStmt(e) -> sprintf "%A;" e
            | Jump(e,labels) -> sprintf "JMP %A, (candidates: %A)" e labels
            | ConditionalJump(cond,t,f) -> sprintf "if %A then\nJMP %A\nelse JMP %A" cond t f
            | Sequence(s1,s2) -> sprintf "%A\n%A" s1 s2
            | MarkLabel(l) -> sprintf "%A:" l
            | Nop -> "NOP"

type Program = 
    Expression of Expr
    | Statement of Statement
    | Branch of (Temporary.Label * Temporary.Label -> Statement)

let unEx (program: Program) : Expr =
    match program with
    | Expression(expr) -> expr
    | Branch(generator) -> 
        let r = Temporary.newTemporary()
        let t = Temporary.newLabel()
        let f = Temporary.newLabel()
        ExprSequence
            ([
                Move(Temp(r),Const(1));
                generator(t,f);
                MarkLabel(f);
                Move(Temp(r),Const(0));
                MarkLabel(t);
             ]
             |> List.fold (fun s s' -> Sequence(s,s')) Nop,Temp(r))
    | Statement(s) -> ExprSequence(s,Const(0))

let unStmt (program: Program) : Statement =
    match program with
    | Expression(expr) -> ExprStmt(expr)
    | Statement(stmt) -> stmt
    | Branch(generator) -> 
        let exit = Temporary.newLabel()
        generator(exit,exit)

let unBranch (program: Program) : (Temporary.Label * Temporary.Label -> Statement) =
    match program with
    | Expression(Const(1)) -> fun (t,_) -> Jump(LabelRef(t),[t])
    | Expression(Const(0)) -> fun (_,f) -> Jump(LabelRef(f),[f])
    | Expression(expr) -> fun (t,f) -> ConditionalJump(expr,t,f)
    | Statement(_) -> failwith "Tiger book says this never happen"
    | Branch(generator) -> generator
