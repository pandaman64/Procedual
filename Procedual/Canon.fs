module Canon

// to suppress incomplete pattern match warning
// (if there were dependent type to declare both list have the same length!)
#nowarn "0025"

let commute (stmt: IR.Statement) (expr: IR.Expr) : bool =
    match stmt,expr with
    | IR.ExprStmt(IR.Const(_)),_ -> true
    | _,IR.Const(_) -> true
    | _,IR.LabelRef(_) -> true
    | _ -> false

let rec reorder (exprs: IR.Expr list) : IR.Statement * IR.Expr list = 
    match exprs with
    | [] -> IR.ExprStmt(IR.Const(0)),[]
    | (IR.Call(_) as e) :: exprs -> 
        let t = Temporary.newTemporary()
        reorder (IR.ESeq(IR.Move(IR.Temp(t),e),IR.Temp(t)) :: exprs)
    | e :: exprs -> 
        let s,e = doExpr e
        let s',exprs = reorder exprs
        if commute s' e
        then
            IR.Sequence(s,s'),e :: exprs
        else
            let t = Temporary.newTemporary()
            IR.Sequence(IR.Sequence(s,IR.Move(IR.Temp(t),e)),s'),
            IR.Temp(t) :: exprs
and reorderStmt (exprs: IR.Expr list) (builder: IR.Expr list -> IR.Statement) : IR.Statement =
    let s,exprs = reorder exprs
    IR.Sequence(s,builder exprs)
and doStmt (stmt: IR.Statement) =
    match stmt with
    | IR.Sequence(s1,s2) -> IR.Sequence(doStmt s1,doStmt s2)
    | IR.Jump(e,labels) -> reorderStmt [e] (fun [e] -> IR.Jump(e,labels))
    | IR.ConditionalJump(e,t,f) -> reorderStmt [e] (fun [e] -> IR.ConditionalJump(e,t,f))
    | IR.Move(IR.Temp(t),IR.Call(f,xs)) -> 
        reorderStmt (f :: xs) (fun (f :: xs) -> IR.Move(IR.Temp(t),IR.Call(f,xs))) 
    | IR.Move(IR.Temp(t),e) -> reorderStmt [e] (fun [e] -> IR.Move(IR.Temp(t),e))
    | IR.Move(IR.Mem(e),x) -> reorderStmt [e;x] (fun [e;x] -> IR.Move(IR.Mem(e),x))
    | IR.Move(IR.ESeq(s,e),e') -> doStmt (IR.Sequence(s,IR.Move(e,e')))
    | IR.ExprStmt(IR.Call(f,xs)) -> reorderStmt (f :: xs) (fun (f :: xs) -> IR.ExprStmt(IR.Call(f,xs)))
    | IR.ExprStmt(e) -> reorderStmt [e] (fun [e] -> IR.ExprStmt(e))
    | s -> reorderStmt [] (fun [] -> s)
and reorderExpr (exprs: IR.Expr list) (builder: IR.Expr list -> IR.Expr) : IR.Statement * IR.Expr =
    let s,exprs = reorder exprs
    s,builder exprs
and doExpr (expr: IR.Expr) =
    match expr with
    | IR.BinaryOp(lhs,op,rhs) -> reorderExpr [lhs;rhs] (fun [lhs;rhs] -> IR.BinaryOp(lhs,op,rhs))
    | IR.Mem(e) -> reorderExpr [e] (fun [e] -> IR.Mem(e))
    | IR.ESeq(s,e) -> 
        let s = doStmt s
        let s',e = doExpr e
        IR.Sequence(s,s'),e
    | IR.Call(f,xs) -> reorderExpr (f :: xs) (fun (f :: xs) -> IR.Call(f,xs))
    | e -> reorderExpr [] (fun [] -> e)

let linearize (stmt: IR.Statement) : IR.Statement list = 
    let rec impl (stmt: IR.Statement) (stmts: IR.Statement list) : IR.Statement list =
        match stmt with
        | IR.Sequence(s1,s2) -> impl s1 (impl s2 stmts)
        | IR.ExprStmt(IR.Const(_)) -> stmts
        | _ -> stmt :: stmts
    impl (doStmt stmt) []