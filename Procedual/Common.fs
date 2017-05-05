module Common

type Var = Var of string

type Unique<'a> = { id: int; value: 'a }
with
    member this.Value = this.value
type UniqueBuilder() =
    let mutable count = 0
    member this.Allocate v =
        count <- count + 1
        { id = count; value = v }
let builder = new UniqueBuilder()
let newUnique v = builder.Allocate v

[<StructuredFormatDisplayAttribute("{AsString}")>]
type Name = 
    LocalName of Unique<Var>
    | ExternalName of Var
with
    member this.AsString =
        match this with
        | LocalName(v) -> 
            let (Var(name)) = v.Value
            sprintf "%s@%d" name v.id
        | ExternalName(Var(name)) -> name

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
    | Assign
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
        | Assign -> ":="