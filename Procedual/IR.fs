module IR

open Common

type Expr =
    Const of int
    | LabelRef of Temporary.Label
    | Temp of Temporary.Temporary
    | BinaryOp of Expr * Op * Expr
    | Call of Expr * Expr list
    | ESeq of Statement list * Expr
and Statement =
    Move of Expr * Expr
    | ExprStmt of Expr
    | Jump of Expr * Temporary.Label list
    | ConditionalJump of Expr * Temporary.Label * Temporary.Label
    | Sequence of Statement * Statement
    | MarkLabel of Temporary.Label

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
        ESeq([
                Move(Temp(r),Const(1));
                generator(t,f);
                MarkLabel(f);
                Move(Temp(r),Const(0));
                MarkLabel(t);
             ],Temp(r))
    | Statement(s) -> ESeq([s],Const(0))

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

module TC = TypeCheck
