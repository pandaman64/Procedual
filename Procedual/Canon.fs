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
    | [] -> IR.Nop,[]
    | (IR.Call(_) as e) :: exprs -> 
        let t = Temporary.newTemporary()
        reorder (IR.ExprSequence(IR.Move(IR.Temp(t),e),IR.Temp(t)) :: exprs)
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
    | IR.Move(IR.ExprSequence(s,e),e') -> doStmt (IR.Sequence(s,IR.Move(e,e')))
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
    | IR.ExprSequence(s,e) -> 
        let s = doStmt s
        let s',e = doExpr e
        IR.Sequence(s,s'),e
    | IR.Call(f,xs) -> reorderExpr (f :: xs) (fun (f :: xs) -> IR.Call(f,xs))
    | e -> reorderExpr [] (fun [] -> e)

let linearize (stmt: IR.Statement) : IR.Statement list = 
    let rec impl (stmt: IR.Statement) (stmts: IR.Statement list) : IR.Statement list =
        match stmt with
        | IR.Sequence(s1,s2) -> impl s1 (impl s2 stmts)
        | IR.ExprStmt(IR.Const(_))
        | IR.Nop -> stmts
        | _ -> stmt :: stmts
    impl (doStmt stmt) []

type Block = IR.Statement list

let toBasicBlock (stmts: IR.Statement list) (epilogueEntry: Temporary.Label) : (Temporary.Label * Block) list =
    let dontEscapeBlock (stmt: IR.Statement) : bool =
        match stmt with
        | IR.Jump(_,_)
        | IR.ConditionalJump(_,_,_) -> false
        | _ -> true
    let rec split pred xs =
        let rec impl before xs =
            match xs with
            | [] -> before,[]
            | x :: xs when not (pred x) -> x :: before,xs
            | x :: xs -> impl (x :: before) xs
        match xs with
        | [] -> []
        | _ ->
            let before,after = impl [] xs
            List.rev before :: split pred after
    let addLabel block =
        match block with
        | [] -> failwith "something wrong"
        | IR.MarkLabel(l) :: _ -> l,block
        | _ -> 
            let l = Temporary.newLabel()
            l,IR.MarkLabel(l) :: block
    let rec addJump blocks =
        match blocks with
        | [] -> []
        | (l1,b1) :: ((l,_) as b2) :: blocks -> 
            let b1 = 
                if dontEscapeBlock (List.last b1)
                then
                    List.append b1 [IR.Jump(IR.LabelRef(l),[l])]
                else
                    b1
            (l1,b1) :: addJump (b2 :: blocks)
        | [l,b] ->
            if dontEscapeBlock (List.last b)
            then
                l,List.append b [IR.Jump(IR.LabelRef(epilogueEntry),[epilogueEntry])]
            else
                l,b
            |> List.singleton

    split dontEscapeBlock stmts
    |> List.map addLabel
    |> addJump

type Trace = Block list

let scheduleTrace (blocks: (Temporary.Label * Block) list) (epilogueEntry: Temporary.Label) : Trace list =
    let rec splitLast xs =
        match xs with
        | [] -> failwith "must have at least one element"
        | [x] -> [],x
        | x :: xs -> 
            let xs,last = splitLast xs
            x :: xs,last
    let visited =
        List.map (fun (l,b) -> l,(b,ref false)) blocks
        |> Map.ofList
    let marked l =
        if l = epilogueEntry
        then
            true
        else
            let (_,visited) = Map.find l visited
            !visited
    let rec concatBlock l =
        let (b,visited) = Map.find l visited
        visited := true
        let last = List.last b
        match last with
        | IR.Jump(_,l :: _) ->
            if marked l
            then
                [b]
            else
                b :: concatBlock l
        | IR.Jump(_,[]) -> failwith "empty jump is not allowed"
        | IR.ConditionalJump(_,_,f) ->
            if marked f
            then
                let f' = Temporary.newLabel()
                let successor = [ IR.MarkLabel(f'); IR.Jump(IR.LabelRef(f),[f]) ]
                [ b; successor ]
            else
                b :: concatBlock f
        | _ -> failwith "basic block must end with (c)jump"
    let folder traces (l,_) =
        let (_,visited) = Map.find l visited
        if !visited
        then
            traces
        else
            concatBlock l :: traces

    List.fold folder [] blocks
    |> List.rev
