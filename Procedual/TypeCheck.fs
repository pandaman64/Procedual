module TypeCheck

open Common

type Type = 
    TConstructor of Var
    | TArrow of Type * Type
with
    member this.Size = 1 // treat only 1 word variables now

let intType = TConstructor(Var("Int"))
let boolType = TConstructor(Var("Bool"))
let unitType = TConstructor(Var("Unit"))

let rec parseSignature (signature: Parser.Signature) =
    match signature with
    | Parser.SigLiteral(v) -> TConstructor(v)
    | Parser.SigArrow(f,x) -> TArrow(parseSignature f,parseSignature x)

type VarDecl = { name: Name; signature: Type }
with
    member this.AsString = 
        sprintf "%A : %A" this.name this.signature

let parseVarDecl (decl: AlphaTransform.VarDecl) : VarDecl =
    { name = decl.name; signature = parseSignature decl.signature }

type Expr = { expr: IR.Program; type_: Type }

type Declaration = {
    name: Name;
    entryPoint: Temporary.Label;
    body: Canon.Block list;
    epilogueEntry: Temporary.Label;
    type_: Type
}

let checkType (t1: Type) (t2: Type) : unit =
    match t1,t2 with
    | t1,t2 when t1 <> t2 -> failwithf "type mismatch between %A and %A" t1 t2
    | _ -> ignore "nothing"

let rec checkExpr (env: Map<Name,Type>) (accesses: Map<Name,Frame.Access>) (frame: Frame.Frame) (expr: AlphaTransform.Expr) : Expr =
    let chk = checkExpr env accesses frame
    match expr with
    | AlphaTransform.Number(x) -> { expr = IR.Expression(IR.Const(x)); type_ = intType }
    | AlphaTransform.Bool(true) -> { expr = IR.Expression(IR.Const(1)); type_ = boolType }
    | AlphaTransform.Bool(false) -> { expr = IR.Expression(IR.Const(0)); type_ = boolType }
    | AlphaTransform.BinaryOp(lhs,op,rhs) ->
        let lhs = chk lhs
        let rhs = chk rhs
        
        checkType lhs.type_ intType
        checkType rhs.type_ intType

        let retType =
            match op with
            | Add | Subtract | Multiply | Divide -> intType
            | Equal | NotEqual
            | GreaterThan | GreaterThanOrEq
            | LessThan | LessThanOrEq -> boolType
            | Assign -> unitType
        
        let expr =
            match op with
            | Assign -> IR.Statement(IR.Move(IR.unEx lhs.expr,IR.unEx rhs.expr))
            | _ -> IR.Expression(IR.BinaryOp(IR.unEx lhs.expr,op,IR.unEx rhs.expr))

        { expr = expr; type_ = retType }
    | AlphaTransform.Call(f,xs) ->
        let f = chk f
        let xs = List.map chk xs
        
        let retType =
            let folder type_ (x: Expr) =
                match type_ with
                | TArrow(xType,retType) ->
                    checkType x.type_ xType
                    retType
                | _ -> failwithf "not a function"
            List.fold folder f.type_ xs

        { expr = IR.Expression(IR.Call(IR.unEx f.expr,xs |> List.map (fun x -> IR.unEx x.expr))); type_ = retType }
    | AlphaTransform.If(cond,ifTrue,ifFalse) ->
        let cond = chk cond
        let ifTrue = chk ifTrue
        let ifFalse = chk ifFalse

        checkType cond.type_ boolType
        checkType ifTrue.type_ ifFalse.type_

        let exit = Temporary.newLabel()
        let t = Temporary.newLabel()
        let f = Temporary.newLabel()
        let ret = Temporary.newTemporary()
        let generator = IR.unBranch cond.expr
        let expr = 
            [
                IR.MarkLabel(f);
                IR.Move(IR.Mem(IR.Temp(ret)),IR.unEx ifFalse.expr)
                IR.Jump(IR.LabelRef(exit),[exit]);
                IR.MarkLabel(t);
                IR.Move(IR.Mem(IR.Temp(ret)),IR.unEx ifTrue.expr)
                IR.Jump(IR.LabelRef(exit),[exit]);
                IR.MarkLabel(exit);
            ]
            |> List.fold (fun p p' -> IR.Sequence(p,p')) (generator(t,f))
            |> fun s -> IR.ExprSequence(s,IR.Mem(IR.Temp(ret)))
            |> IR.Expression

        { expr = expr; type_ = ifTrue.type_ }
    | AlphaTransform.Let(name,value,body) ->
        let name = parseVarDecl name

        let env = Map.add name.name name.signature env
        let access = frame.AllocLocal name.signature.Size true

        let value = checkExpr env accesses frame value
        checkType name.signature value.type_

        let accesses = Map.add name.name access accesses

        let body = checkExpr env accesses frame body

        let expr =
            IR.ExprSequence(
                IR.Move(frame.AccessVar access,IR.unEx value.expr),
                IR.unEx body.expr
            )

        { expr = IR.Expression(expr); type_ = body.type_ }
    | AlphaTransform.Sequence(exprs) ->
        let exprs = List.map chk exprs
        let expr =
            let rec make stmt (exprs: Expr list) =
                match exprs with
                | [] -> failwith "empty sequence"
                | [ expr ] -> stmt,expr.expr
                | expr :: exprs -> make (IR.Sequence(stmt,IR.ExprStmt(IR.unEx expr.expr))) exprs
            let stmts,expr = make (IR.ExprStmt(IR.Const(0))) exprs
            IR.ExprSequence(stmts,IR.unEx expr)
        { expr = IR.Expression(expr); type_ = (List.last exprs).type_ }
    | AlphaTransform.Ref(v) ->
        match accesses.TryFind v with
        | Some(access) ->
            { 
                expr = IR.Expression(frame.AccessVar access); 
                type_ = Map.find v env 
            }
        | None -> failwithf "access to %A not found in %A" v accesses

let checkDecl (env: Map<Name,Type>) (accesses: Map<Name,Frame.Access> ref) (decl: AlphaTransform.Declaration) : Declaration =
    match decl with
    | AlphaTransform.ValueDeclaration(name,body) -> 
        failwith "sonouchi kangaeru"
    | AlphaTransform.FunctionDeclaration(name,arguments,retType,body) -> 
        let arguments = arguments |> List.map parseVarDecl
        let retType = parseSignature retType
        let funType = List.foldBack (fun (decl: VarDecl) retType -> TArrow(decl.signature,retType)) arguments retType

        let env = 
            arguments
            |> List.fold (fun env decl -> Map.add decl.name decl.signature env) env
            |> Map.add name funType

        let frame =
            let name = Var(name.AsString)
            let arguments = arguments |> List.map (fun arg -> arg.signature.Size,true)
            //let framePointer = Frame.stackPointer
            let framePointer = Temporary.newTemporary()
            Frame.Frame(name,arguments,framePointer)

        let label = Temporary.newLabel()
        accesses := Map.add name (Frame.Access.Literal(IR.LabelRef(label))) !accesses

        let accesses = 
            List.zip arguments frame.arguments
            |> List.fold (fun accesses (arg,access) -> Map.add arg.name access accesses) !accesses
        
        let body = checkExpr env accesses frame body
        checkType body.type_ retType

        let r4 = Temporary.newTemporary()
        let r5 = Temporary.newTemporary()
        let prologue = 
            [
                IR.MarkLabel(label);
                // save callee-save registers
                IR.Move(IR.Temp(r4),IR.Temp(Frame.registers.Item 4));
                IR.Move(IR.Temp(r5),IR.Temp(Frame.registers.Item 5));
            ]
            |> List.fold (fun s s' -> IR.Sequence(s,s')) IR.Nop
        let body = IR.Move(IR.Temp(Frame.returnValue),IR.unEx body.expr)
        let epilogueEntry = Temporary.newLabel()
        let epilogue =
            [
                IR.MarkLabel(epilogueEntry);
                // restore callee-save registers
                IR.Move(IR.Temp(Frame.registers.Item 4),IR.Temp(r4));
                IR.Move(IR.Temp(Frame.registers.Item 5),IR.Temp(r5));
                // jump back to callee
                IR.Jump(IR.Temp(Frame.returnAddress),[(*empty is ok?*)]);
            ] 
            |> List.fold (fun s s' -> IR.Sequence(s,s')) IR.Nop
        
        let body = 
            let body = 
                //Canon.linearize (IR.Sequence(prologue,body))
                Canon.linearize body
                |> fun stmts -> Canon.toBasicBlock stmts epilogueEntry
                |> fun stmts -> Canon.scheduleTrace stmts epilogueEntry
                |> List.reduce List.append
            List.concat
                [
                    [Canon.linearize prologue]
                    body;
                    [Canon.linearize epilogue];
                ]

        {
            name = name;
            entryPoint = label;
            body = body;
            epilogueEntry = Temporary.newLabel();
            type_ = funType
        }

let checkDecls (env: Map<Name,Type>) (decls: AlphaTransform.Declaration list) : Declaration list = 
    let accesses = ref Map.empty
    let mapFolder env (decl: AlphaTransform.Declaration) =
        printfn "%A" decl.Name
        let decl = checkDecl env accesses decl
        let access = Frame.Access.Literal(IR.LabelRef(decl.entryPoint))
        decl,(Map.add decl.name decl.type_ env)
    List.mapFold mapFolder env decls
    |> fst