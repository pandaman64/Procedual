module Parser

open Common
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