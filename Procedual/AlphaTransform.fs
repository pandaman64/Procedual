module AlphaTransform

open Common

type Signature = Parser.Signature

type VarDecl = { name: Name; signature: Signature }
with
    member this.AsString = 
        sprintf "%A : %A" this.name this.signature

type Expr =   
    Number of int
    | Bool of bool
    | BinaryOp of Expr * Op * Expr
    | Ref of Name
    | Let of VarDecl * Expr * Expr
    | Call of Expr * Expr list
    | If of Expr * Expr * Expr
    | Sequence of Expr list

type Declaration = 
    ValueDeclaration of VarDecl * Expr
    | FunctionDeclaration of Name * VarDecl list * Signature * Expr

type Environment = Map<Var,Name>

let rec transformExpr (env: Environment) (expr: Parser.Expr) : Expr =
    match expr with
    | Parser.Number(x) -> Number(x)
    | Parser.Bool(x) -> Bool(x)
    | Parser.BinaryOp(lhs,op,rhs) ->
        BinaryOp(transformExpr env lhs,op,transformExpr env rhs)
    | Parser.Call(f,xs) ->
        Call(transformExpr env f,List.map (transformExpr env) xs)
    | Parser.If(cond,ifTrue,ifFalse) ->
        If(transformExpr env cond,transformExpr env ifTrue,transformExpr env ifFalse)
    | Parser.Let(name,value,body) ->
        let value = transformExpr env value
        
        let name' = LocalName(newUnique name.name)
        let env = Map.add name.name name' env
        let body = transformExpr env body

        Let({ name = name'; signature = name.signature },value,body)
    | Parser.Ref(v) ->
        match env.TryFind v with
        | None -> failwithf "variable %A not found in %A" v env
        | Some(v) -> Ref(v)
    | Parser.Sequence(exprs) ->
        Sequence(List.map (transformExpr env) exprs)

let transformDecl (env: Environment) (decl: Parser.Declaration) : Declaration =
    match decl with
    | Parser.ValueDeclaration(decl,value) ->
        ValueDeclaration({ name = ExternalName(decl.name); signature = decl.signature },transformExpr env value)
    | Parser.FunctionDeclaration(name,arguments,retType,value) ->
        let name' = ExternalName(name)
        let arguments' = arguments |> List.map (fun arg -> { name = LocalName(newUnique arg.name); signature = arg.signature })
        let env = 
            List.zip arguments arguments'
            |> List.fold (fun env (arg,arg') -> Map.add arg.name arg'.name env) env
        FunctionDeclaration(name',arguments',retType,transformExpr env value)

let transformDecls (decls: Parser.Declaration list) : Declaration list =
    let mapFold env (decl: Parser.Declaration) =
        let name' = ExternalName(decl.Name)
        let env = Map.add decl.Name name' env
        transformDecl env decl,env
    List.mapFold mapFold Map.empty decls
    |> fst
