module Temporary

open Common

[<StructuredFormatDisplayAttribute("{AsString}")>]
type Temporary = Temp of Unique<unit>
with
    member this.AsString = 
        let (Temp(t)) = this
        sprintf "t%d" t.id
[<StructuredFormatDisplayAttribute("{AsString}")>]
type Label = Label of Unique<string>
with
    member this.AsString =
        let (Label(l)) = this
        sprintf "%s%d" l.value l.id

let newLabel() = Label(newUnique "L")
let newTemporary() = Temp(newUnique ())
