module Common

type Var = Var of string

type Unique<'a> = { id: int; value: 'a }
with
    member this.Value = this.value
let newUnique = 
    let mutable counter = 0
    let impl v =
        counter <- counter + 1
        { id = counter; value = v }
    impl

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