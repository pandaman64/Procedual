module Temporary

open Common

type Temporary = Unique<unit>

type Label = Unique<string>

type Level = Level of int

let newLabel() = newUnique "L"
let newTemporary() = newUnique ()
let newLabel name = newUnique name

type Translate() =
    let level = Level(0)
