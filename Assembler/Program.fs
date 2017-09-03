
open FParsec

type UnaryOp =
    | SL
    | SR
    | JR
with
    member this.AsOpcode =
        match this with
        | SL -> "00111"
        | SR -> "01000"
        | JR -> "10011"
    member this.AsString = 
        match this with 
        | SL -> "SL"
        | SR -> "SR"
        | JR -> "JR"

type BinOp =
    | ADD
    | SUB
    | MUL
    | DIV
    | AND
    | OR
    | EQ
    | NEQ
    | GT
    | GEQ
    | LT
    | LEQ
    | ST
    | LD
    | MV 
with
    member this.AsOpcode =
        match this with
        | ADD -> "00011"
        | SUB -> "00100"
        | MUL -> "00101"
        | DIV -> "00110"
        | AND -> "01001"
        | OR  -> "01010"
        | EQ  -> "01011"
        | NEQ -> "01100"
        | GT  -> "01101"
        | GEQ -> "01110"
        | LT  -> "01111"
        | LEQ -> "10000"
        | ST  -> "10001"
        | LD  -> "10010"
        | MV  -> "00010"
    member this.AsString =
        match this with
        | ADD -> "ADD"
        | SUB -> "SUB"
        | MUL -> "MUL"
        | DIV -> "DIV"
        | AND -> "AND"
        | OR  -> "OR"
        | EQ  -> "EQ"
        | NEQ -> "NEQ"
        | GT  -> "GT"
        | GEQ -> "GEQ"
        | LT  -> "LT"
        | LEQ -> "LEQ"
        | ST  -> "ST"
        | LD  -> "LD"
        | MV  -> "MV"

type ImmediateOp =
    | LDI
    | LDIU
    | LDHI
    | ADDI
    | ADDIU
with
    member this.AsOpcode = 
        match this with 
        | LDI   -> "01000"
        | LDIU  -> "01001"
        | LDHI  -> "01010"
        | ADDI  -> "01100"
        | ADDIU -> "01101"
    member this.AsString = 
        match this with 
        | LDI   -> "LDI"
        | LDIU  -> "LDIU"
        | LDHI  -> "LDHI"
        | ADDI  -> "ADDI"
        | ADDIU -> "ADDIU"
    
type JumpOp =
    | J
    | JAL
with
    member this.AsOpcode =
        match this with
        | J   -> "10100"
        | JAL -> "10101"
    member this.AsString =
        match this with
        | J   -> "J"
        | JAL -> "JAL"

type ConditionalJumpOp = 
    | BEZ
    | BNZ
with
    member this.AsOpcode =
        match this with
        | BEZ -> "10000"
        | BNZ -> "10001"
    member this.AsString =
        match this with
        | BEZ -> "BEZ"
        | BNZ -> "BNZ"

type Reg = Reg of int
with
    member this.AsOperand =
        let (Reg x) = this
        match x with
        | 0 -> "000"
        | 1 -> "001"
        | 2 -> "010"
        | 3 -> "011"
        | 4 -> "100"
        | 5 -> "101"
        | 6 -> "110"
        | 7 -> "111"
        | _ -> failwith "something wrong"
    member this.AsString =
        let (Reg x) = this
        match x with
        | 0 -> "r0"
        | 1 -> "r1"
        | 2 -> "r2"
        | 3 -> "r3"
        | 4 -> "r4"
        | 5 -> "r5"
        | 6 -> "r6"
        | 7 -> "r7"
        | _ -> failwith "something wrong"

type Statement =
    | Label of string * int
    | UnaryOp of UnaryOp * Reg
    | BinOp of BinOp * Reg * Reg
    | ImmOp of ImmediateOp * Reg * int
    | JumpOp of JumpOp * string * int
    | ConditionalJumpOp of ConditionalJumpOp * Reg * string * int

type Env = {
    labels: Map<string, int>
    statements: Statement list
}

type State = {
    count: int
    labels: Map<string, int>
}
let increment (state: State): State = 
    {
        count = state.count + 1
        labels = state.labels
    }
let addLabel (name: string) (position: int) (state: State): State =
    {
        count = state.count
        labels = Map.add name position state.labels
    }
let emptyState = 
    {
        count = 0
        labels = Map.empty
    }
type Parser<'a> = Parser<'a, State>

let pLabelName: Parser<string> = 
    let cond (c: char) = System.Char.IsLetterOrDigit c
    many1Satisfy cond

let pLabelDecl: Parser<Statement> = parse {
    let! labelName = pLabelName .>> pstring ":" .>> spaces
    let! state = getUserState
    let position = state.count
    do! updateUserState (addLabel labelName position)
    return Label(labelName, position)
}

let pReg: Parser<Reg> =
    pchar 'r' >>. pint32 .>> spaces
    |>> Reg

let pMapping (m: (string * 'a) list): Parser<'a> =
    let parsers =
        m
        |> List.map (fun (name,value) -> pstring name >>% value)
    choice parsers .>> spaces

let pUnaryOp: Parser<Statement> =
    let mapping = 
        [
            "SL", SL
            "SR", SR
            "JR", JR
        ]
    tuple2 (pMapping mapping) pReg 
    |>> UnaryOp

let pBinOp: Parser<Statement> =
    let mapping = 
        [
            "ADD", ADD
            "SUB", SUB
            "MUL", MUL
            "DIV", DIV
            "AND", AND
            "OR", OR
            "EQ", EQ
            "NEQ", NEQ
            "GT", GT
            "GEQ", GEQ
            "LT", LT
            "LEQ", LEQ
            "ST", ST
            "LD", LD
            "MV", MV
        ]
    tuple3 (pMapping mapping) (pReg .>> pchar ',' .>> spaces) pReg 
    |>> BinOp

let pImmediateOp: Parser<Statement> =
    let mapping = 
        [
            "LDIU", LDIU
            "LDHI", LDHI
            "LDI", LDI
            "ADDIU", ADDIU
            "ADDI", ADDI
        ]
    tuple3 (pMapping mapping) (pReg .>> pchar ',' .>> spaces) pint32 
    |>> ImmOp

let pJumpOp: Parser<Statement> =
    let mapping = 
        [
            "JAL", JAL
            "J", J
        ]
    tuple3 (pMapping mapping) pLabelName (getUserState |>> (fun state -> state.count))
    |>> JumpOp
    
let pConditionalJumpOp: Parser<Statement> =
    let mapping = 
        [
            "BEZ", BEZ
            "BNZ", BNZ
        ]
    tuple4 
        (pMapping mapping)
        (pReg .>> pchar ',' .>> spaces)
        pLabelName
        (getUserState |>> (fun state -> state.count))
    |>> ConditionalJumpOp

let pOperation: Parser<Statement> = 
    choice 
        [
            attempt pUnaryOp
            attempt pBinOp
            attempt pImmediateOp
            attempt pConditionalJumpOp
            pJumpOp
        ]
    .>> spaces
    .>> updateUserState increment

let pStatement: Parser<Statement> =
    (attempt pLabelDecl) <|> pOperation

let pStatements = many pStatement .>> eof

let bitRepresentation (x: int) (digits: int): string =
    if x < -(1 <<< (digits - 1)) || x >= (1 <<< (digits - 1)) then
        failwithf "%d out of range: [%d, %d]" x -(1 <<< (digits - 1)) (1 <<< (digits - 1))
    let x' = if x >= 0 then x else (1 <<< (digits - 1)) + x
    let mutable ret = ""
    for i in [0..(digits - 2)] do
        if (x' &&& (1 <<< i)) <> 0 then
            ret <- "1" + ret
        else
            ret <- "0" + ret
    ret <- if x >= 0 then "0" + ret else "1" + ret
    ret

let bitUnsignedRepresentation (x: int) (digits: int): string =
    if x < 0 || x >= (1 <<< digits) then
        failwithf "%d out of range: [%d, %d]" x 0 (1 <<< digits)
    let mutable ret = ""
    for i in [0..(digits - 1)] do
        if (x &&& (1 <<< i)) <> 0 then
            ret <- "1" + ret
        else
            ret <- "0" + ret
    ret

let eightBitRepresentation (x: int): string =
    bitRepresentation x 8

let elevenBitRepresentation (x: int): string = 
    bitRepresentation x 11

let eightBitUnsignedRepresentation (x: int): string =
    bitUnsignedRepresentation x 8

let choiceRepresentation (immOp: ImmediateOp) (digits: int): (int -> string) =
    match immOp with
    | LDIU | ADDIU -> fun x -> bitUnsignedRepresentation x digits
    | _ -> fun x -> bitRepresentation x digits

[<EntryPoint>]
let main argv = 
    let fileName = 
        if argv.Length = 0
        then
            "main.asm"
        else
            (argv.GetValue 0) :?> string
    let contents = System.IO.File.ReadAllText(fileName)
    match runParserOnFile pStatements emptyState fileName (new System.Text.UTF8Encoding()) with
    | Success(result, state, _) -> 
        let mapper (stmt: Statement): string =
            match stmt with
            | Label(name, position) -> sprintf "// %s: (at position %d)" name position
            | UnaryOp(op, reg) -> 
                sprintf "00000_%s_000_%s //%s %s" 
                    reg.AsOperand op.AsOpcode
                    op.AsString reg.AsString
            | BinOp(op, dst, src) -> 
                sprintf "00000_%s_%s_%s //%s %s, %s"
                    dst.AsOperand src.AsOperand op.AsOpcode
                    op.AsString dst.AsString src.AsString
            | ImmOp(op, dst, value) ->
                sprintf "%s_%s_%s //%s %s, %d" 
                    op.AsOpcode dst.AsOperand (choiceRepresentation op 8 value)
                    op.AsString dst.AsString value
            | JumpOp(op, label, position) ->
                match state.labels.TryFind label with
                | None -> failwithf "label %s not found in %A" label state.labels
                | Some(labelPosition) ->
                    sprintf "%s_%s //%s %s" 
                        op.AsOpcode (elevenBitRepresentation (labelPosition - position - 1))
                        op.AsString label
            | ConditionalJumpOp(op, src, label, position) ->
                match state.labels.TryFind label with
                | None -> failwithf "label %s not found in %A" label state.labels
                | Some(labelPosition) ->
                    sprintf "%s_%s_%s //%s %s, %s" 
                        op.AsOpcode src.AsOperand (eightBitRepresentation (labelPosition - position - 1))
                        op.AsString src.AsString label
        for line in List.map mapper result do
            printfn "%s" line
    | Failure(err, _, _) -> printfn "error: %s" err
    0 // 整数の終了コードを返します
