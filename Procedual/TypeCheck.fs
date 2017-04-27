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

type UntypedExpr =   
    Number of int
    | Bool of bool
    | BinaryOp of Expr * Op * Expr
    | Ref of Name
    | Let of VarDecl * Expr * Expr
    | Call of Expr * Expr list
    | Fun of VarDecl list * Expr
    | If of Expr * Expr * Expr
    | Sequence of Expr list
and
    Expr = { expr: UntypedExpr; type_: Type }

type UntypedDeclaration =
    ValueDeclaration of VarDecl * Expr
    | FunctionDeclaration of Name * VarDecl list * Expr
type Declaration = { decl: UntypedDeclaration; type_: Type }
with
    member this.Name = 
        match this.decl with
        | ValueDeclaration(decl,_) -> decl.name
        | FunctionDeclaration(name,_,_) -> name

let checkType (t1: Type) (t2: Type) : unit =
    match t1,t2 with
    | t1,t2 when t1 <> t2 -> failwithf "type mismatch between %A and %A" t1 t2
    | _ -> ignore "nothing"

let rec checkExpr (env: Map<Name,Type>) (expr: AlphaTransform.Expr) : Expr =
    match expr with
    | AlphaTransform.Number(x) -> { expr = Number(x); type_ = intType }
    | AlphaTransform.Bool(b) -> { expr = Bool(b); type_ = boolType }
    | AlphaTransform.BinaryOp(lhs,op,rhs) ->
        match env.TryFind (ExternalName(Var(op.AsString))) with
        | None -> failwithf "op %A not found in %A" op env
        | Some(TArrow(lhsType,TArrow(rhsType,retType))) ->
            let lhs = checkExpr env lhs
            let rhs = checkExpr env rhs
            checkType lhs.type_ lhsType
            checkType rhs.type_ rhsType
            { expr = BinaryOp(lhs,op,rhs); type_ = retType }
        | _ -> failwithf "%A is not a binary operator" expr
    | AlphaTransform.Call(f,xs) ->
        let f = checkExpr env f
        let xs = List.map (checkExpr env) xs
        
        let retType =
            let folder type_ (x: Expr) =
                match type_ with
                | TArrow(xType,retType) ->
                    checkType x.type_ xType
                    retType
                | _ -> failwithf "not a function"
            List.fold folder f.type_ xs

        { expr = Call(f,xs); type_ = retType }
    | AlphaTransform.If(cond,ifTrue,ifFalse) ->
        let cond = checkExpr env cond
        let ifTrue = checkExpr env ifTrue
        let ifFalse = checkExpr env ifFalse

        checkType cond.type_ boolType
        checkType ifTrue.type_ ifFalse.type_

        { expr = If(cond,ifTrue,ifFalse); type_ = ifTrue.type_ }
    | AlphaTransform.Let(name,value,body) ->
        let name = parseVarDecl name
        let env = Map.add name.name name.signature env
        let value = checkExpr env value
        let body = checkExpr env body

        checkType name.signature value.type_
        { expr = Let(name,value,body); type_ = body.type_ }
    | AlphaTransform.Sequence(exprs) ->
        let exprs = List.map (checkExpr env) exprs
        { expr = Sequence(exprs); type_ = (List.last exprs).type_ }
    | AlphaTransform.Ref(v) -> { expr = Ref(v); type_ = Map.find v env }

let checkDecl (env: Map<Name,Type>) (decl: AlphaTransform.Declaration) : Declaration =
    match decl with
    | AlphaTransform.ValueDeclaration(name,body) -> 
        let name = parseVarDecl name
        let env = Map.add name.name name.signature env
        let body = checkExpr env body

        checkType name.signature body.type_
        { decl = ValueDeclaration(name,body); type_ = name.signature }
    | AlphaTransform.FunctionDeclaration(name,arguments,retType,body) -> 
        let arguments = arguments |> List.map parseVarDecl
        let retType = parseSignature retType
        let funType = List.foldBack (fun (decl: VarDecl) retType -> TArrow(decl.signature,retType)) arguments retType

        let env = 
            arguments
            |> List.fold (fun env decl -> Map.add decl.name decl.signature env) env
            |> Map.add name funType

        let body = checkExpr env body
        checkType body.type_ retType

        { decl = FunctionDeclaration(name,arguments,body); type_ = funType }

let checkDecls (env: Map<Name,Type>) (decls: AlphaTransform.Declaration list) : Declaration list = 
    let mapFolder env decl =
        let decl = checkDecl env decl
        (decl,Map.add decl.Name decl.type_ env)
    List.mapFold mapFolder env decls
    |> fst