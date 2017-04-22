// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

open FParsec

type Unique<'a> = { id: int; value: 'a }
let newUnique = 
    let mutable counter = 0
    let impl v =
        counter <- counter + 1
        { id = counter; value = v }
    impl

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    (*fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply*)
    p

type Var = Var of string

[<StructuredFormatDisplayAttribute("{AsString}")>]
type Signature = 
    SigLiteral of Var
    | SigArrow of Signature * Signature
with
    member this.AsString =
        match this with
        | SigLiteral(Var(s)) -> sprintf "%s" s
        | SigArrow(lhs,SigLiteral(Var(rhs))) -> sprintf "%A -> %s" lhs rhs
        | SigArrow(lhs,rhs) -> sprintf "%A -> (%A)" lhs rhs

[<StructuredFormatDisplayAttribute("{AsString}")>]
type VarDecl = { name: Var; signature: Signature }
with
    member this.AsString = 
        let (Var(name)) = this.name
        sprintf "%s : %A" name this.signature

[<StructuredFormatDisplayAttribute("{AsString}")>]
type Op = 
    Add
    | Subtract
    | Multiply
    | Divide
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterThanOrEq
    | LessThan
    | LessThanOrEq
with
    member this.AsString =
        match this with
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Equal -> "="
        | NotEqual -> "<>"
        | GreaterThan -> ">"
        | GreaterThanOrEq -> ">="
        | LessThan -> "<"
        | LessThanOrEq -> "<="

let argumentsToString xs =
    List.map (fun x -> sprintf "%A" x) xs
    |> String.concat ","

[<StructuredFormatDisplayAttribute("{AsString}")>]
type Expr =   
    Number of int
    | Bool of bool
    | BinaryOp of Expr * Op * Expr
    | Ref of Var
    | Let of VarDecl * Expr * Expr
    | Call of Expr * Expr list
    | Fun of VarDecl list * Expr
    | If of Expr * Expr * Expr
with
    member this.AsString = 
        match this with
        | Number(x) -> sprintf "%d" x
        | Bool(x) -> sprintf "%b" x
        | BinaryOp(lhs,op,rhs) -> sprintf "(%A %A %A)" lhs op rhs
        | Ref(Var(v)) -> sprintf "%s" v
        | Let(decl,value,body) -> sprintf "let %A = %A;\n%A" decl value body
        | Call(f,xs) -> 
            sprintf "%A(%s)" f (argumentsToString xs)
        | Fun(arguments,body) ->
            sprintf "(fun %s -> %A)" (argumentsToString arguments) body
        | If(cond,ifTrue,ifFalse) ->
            sprintf "if %A then %A else %A" cond ifTrue ifFalse


[<StructuredFormatDisplayAttribute("{AsString}")>]
type Declaration = 
    ValueDeclaration of VarDecl * Expr
    | FunctionDeclaration of Var * VarDecl list * Expr
with
    member this.Name = 
        match this with
        | ValueDeclaration(decl,_) -> decl.name
        | FunctionDeclaration(name,_,_) -> name
    member this.AsString =
        match this with
        | ValueDeclaration(decl,value) -> sprintf "value %A = %A;" decl value
        | FunctionDeclaration(Var(name),arguments,body) ->
            List.map (sprintf "%A") arguments
            |> String.concat ","
            |> fun arguments -> sprintf "function %s(%s) = %A" name arguments body

module Parser =
    type ExprParser = Parser<Expr,unit>
    type DeclarationParser = Parser<Declaration,unit>

    let pIdentifierVar : Parser<Var,unit> =     
        let first c = System.Char.IsLetter c
        let following c = System.Char.IsLetterOrDigit c
        many1Satisfy2 first following .>> spaces |>> Var
    
    let pSignature : Parser<Signature,unit> =
        let pSignature,pSignatureRef = createParserForwardedToRef()
        let pSimple =
            let pLiteral = pIdentifierVar |>> SigLiteral
            let braced = between (pchar '(' >>. spaces) (pchar ')' >>. spaces) pSignature
            braced <|> pLiteral
        let pArrow = 
            let arrow = pstring "->" >>. spaces >>% fun lhs rhs -> SigArrow(lhs,rhs)
            chainr1 pSimple arrow

        pSignatureRef := pArrow

        pSignature

    let pVarDecl : Parser<VarDecl,unit> = parse{
        let! name = pIdentifierVar
        do! pchar ':' >>. spaces
        let! signature = pSignature
        return { name = name; signature = signature }
    }
    
    let pExpr,pExprRef = createParserForwardedToRef<Expr,unit> ()
    
    let pRef : ExprParser = parse{
        let reserved = [ "let"; "function"; "value"; "if"; "then"; "else" ];
        let! (Var(ident)) = pIdentifierVar
        if List.contains ident reserved then
            failwith "reserved word"
        else
            return Ref(Var(ident))
    }

    let pNumberLiteral : ExprParser = pint32 .>> spaces |>> Number

    let pParen : ExprParser =
        pchar '(' >>. spaces >>. pExpr .>> pchar ')' .>> spaces

    let pLet : ExprParser = parse{
        do! pstring "let" >>. spaces
        let! name = pVarDecl
        do! pchar '=' >>. spaces
        let! value = pExpr
        do! pchar ';' >>. spaces
        let! body = pExpr
        return Let(name,value,body)
    } 

    let pIf : ExprParser = parse{
        do! pstring "if" >>. spaces
        let! cond = pExpr
        do! pstring "then" >>. spaces
        let! ifTrue = pExpr
        do! pstring "else" >>. spaces
        let! ifFalse = pExpr
        return If(cond,ifTrue,ifFalse)
    }

    let pPrimitive : ExprParser = 
        choice[
            pLet <!> "let";
            pIf <!> "if";
            pParen <!> "paren";
            pRef <!> "ref";
            pNumberLiteral <!> "num";
        ]

    let pCall : ExprParser = parse{
        let! callee = pPrimitive
        let! arguments = 
            (attempt (pchar '(' >>. spaces)
            >>. sepBy pExpr (pchar ',' >>. spaces)
            .>> pchar ')' .>> spaces)
            |> opt
        match arguments with
        | Some(arguments) -> return Call(callee,arguments)
        | None -> return callee
    }

    let pOp (ops: (string * Op) seq) : Parser<Expr -> Expr -> Expr,unit> =
        ops
        |> Seq.map (fun (s,op) -> attempt (pstring s >>. spaces >>% fun lhs rhs -> BinaryOp(lhs,op,rhs)))
        |> choice
        <!> "op"
        
    let pMultitive : ExprParser = 
        let op = pOp [ "*",Multiply; "/",Divide ]
        chainl1 pCall op <!> "multitive"

    let pAdditive : ExprParser =
        let op = pOp [ "+",Add; "-",Subtract ]
        chainl1 pMultitive op <!> "additive"

    let pRelational : ExprParser =
        let op = 
            pOp [
                "=",Equal;
                "<>",NotEqual;
                ">=",GreaterThanOrEq;
                ">",GreaterThan;
                "<=",LessThanOrEq;
                "<",LessThan
            ]
        chainl1 pAdditive op <!> "relational"

    pExprRef := spaces >>. pRelational

    let pValue : DeclarationParser = parse{
        do! pstring "value" >>. spaces
        let! varDecl = pVarDecl
        do! pchar '=' >>. spaces
        let! body = pExpr
        do! pchar ';' >>. spaces
        return ValueDeclaration(varDecl,body) 
    }
    let pFunction : DeclarationParser = parse{
        do! pstring "function" >>. spaces
        let! name = pIdentifierVar
        let! arguments = between (pchar '(' >>. spaces) (pchar ')' >>. spaces) (many pVarDecl)
        do! pchar '=' >>. spaces
        let! body = pExpr
        do! pchar ';' >>. spaces
        return FunctionDeclaration(name,arguments,body)
    }

    let pDeclaration : DeclarationParser =
        pValue <|> pFunction

    let pProgram = spaces >>. many1 pDeclaration

type StackFrame = { environment: Map<Var,Expr> }
with
    member this.Add name expr = { environment = Map.add name expr this.environment }
type CallStack = StackFrame list
type ExecutionState = { globalEnvironment: Map<Var,Expr>; callStack: CallStack }
with
    member this.Add name expr =
        let newCallStack = 
            let head = this.callStack.Head
            let tail = this.callStack.Tail
            head.Add name expr :: tail
        { globalEnvironment = this.globalEnvironment; callStack = newCallStack }

let rec bigStep (expr: Expr) (state: ExecutionState) (show: bool) =
    match expr with
    | Number(_)
    | Bool(_)
    | Fun(_,_) -> 
        if show
        then
            printfn "--------\n%A" expr
            ignore (stdin.ReadLine())
        expr,state
    | BinaryOp(lhs,op,rhs) -> 
        let lhs,state = 
            match bigStep lhs state false with
            | Number(lhs),state -> lhs,state
            | _ -> failwith "type mismatch"
        if show
        then
            printfn "--------\n(%d %A %A)" lhs op rhs
            ignore (stdin.ReadLine())
        let rhs,state = 
            match bigStep rhs state false with
            | Number(rhs),state -> rhs,state
            | _ -> failwith "type mismatch"
        if show
        then
            printfn "--------\n(%d %A %d)" lhs op rhs
            ignore (stdin.ReadLine())
        let ret =
            match op with
            | Add -> Number(lhs + rhs)
            | Subtract -> Number(lhs - rhs)
            | Multiply -> Number(lhs * rhs)
            | Divide -> Number(lhs / rhs)
            | Equal -> Bool(lhs = rhs)
            | NotEqual -> Bool(lhs <> rhs)
            | GreaterThan -> Bool(lhs > rhs)
            | GreaterThanOrEq -> Bool(lhs >= rhs)
            | LessThan -> Bool(lhs < rhs)
            | LessThanOrEq -> Bool(lhs <= rhs)
        if show
        then
            printfn "--------\n%A" ret
            ignore (stdin.ReadLine())
        ret,state
    | If(cond,ifTrue,ifFalse) ->
        let cond,state = 
            match bigStep cond state false with
            | Bool(cond),state -> 
                if show
                then
                    printfn "--------\nif %b then %A else %A" cond ifTrue ifFalse
                    ignore (stdin.ReadLine())
                cond,state
            | _ -> failwith "type mismatch"
        if cond
        then
            if show
            then
                printfn "--------\n%A" ifTrue
                ignore (stdin.ReadLine())
            
            bigStep ifTrue state true
        else
            if show
            then
                printfn "--------\n%A" ifFalse
                ignore (stdin.ReadLine())
            
            bigStep ifFalse state true
    | Ref(v) ->
        let stackFrame = state.callStack.Head
        match stackFrame.environment.TryFind v with
        | Some(x) -> 
            bigStep x state false
        | None ->
            match state.globalEnvironment.TryFind v with
            | Some(x) -> 
                bigStep x state false
            | None -> failwithf "%A not found" v
    | Let(varDecl,value,body) ->
        let value,state = bigStep value state false
        printfn "--------\nlet %A = %A;\n%A" varDecl value body
        ignore (stdin.ReadLine())
        let state = state.Add varDecl.name value
        bigStep body state true
    | Call(f,xs) ->
        let arguments,body,state = 
            match bigStep f state false with
            | Fun(arguments,body),state -> 
                printfn "--------\n(fun %s -> %A)(%s)" (argumentsToString arguments) body (argumentsToString xs)
                ignore (stdin.ReadLine())
                arguments,body,state
            | _ -> failwith "type mismatch"
        let state,xs = 
            let folder x (state,xs) =
                let x,state = bigStep x state false
                state,x :: xs
            List.foldBack folder xs (state,[])
        if show then
            printfn "--------\n(fun %s -> %A)(%s)" (argumentsToString arguments) body (argumentsToString xs)
            ignore (stdin.ReadLine())
        let stackFrame = List.zip arguments xs |> List.fold (fun (frame: StackFrame) (param,arg) -> frame.Add param.name arg) { environment = Map.empty }
        let state = { globalEnvironment = state.globalEnvironment; callStack = stackFrame :: state.callStack }
        if show then
            printfn "--------\n%A" body
            ignore (stdin.ReadLine())
        let value,state = bigStep body state false
        let state = { globalEnvironment = state.globalEnvironment; callStack = state.callStack.Tail }
        value,state

let toEnvironment (decls: Declaration list) : Map<Var,Expr> =
    let folder env decl = 
        let value = 
            match decl with
            | ValueDeclaration(_,value) -> value
            | FunctionDeclaration(_,arguments,value) -> Fun(arguments,value)
        Map.add decl.Name value env
    List.fold folder Map.empty decls

[<EntryPoint>]
let main argv = 
    let simple = """value main : Int = 1 + 2 * 3 + 4;"""
    let source = """
function fact(x: Int) =
    if x = 0 then 1
    else x * fact(x - 1);
value main : Int =
    let x : Int = 2;
    fact(x);
"""
    printfn "%s" source
    match run Parser.pProgram source with
    | Success(decls,_,_) -> 
        printfn "%A" decls
        let state = 
            {
                globalEnvironment = decls |> toEnvironment;
                callStack = [ { environment = Map.empty } ];
            }
        let expr = Ref(Var("main"))
        printfn "%A" expr
        let result,_ = bigStep expr state true
        printfn "%A" result
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
