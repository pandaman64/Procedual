module InstructionChoice

open Common

type Operation = {
    op: Op;
    dst: Temporary.Temporary list
    src: Temporary.Temporary list
    jump: Temporary.Label list option
}

type Instruction =
    Operation of Operation
    | Label of Temporary.Label
    | Move of Temporary.Temporary * Temporary.Temporary // dst * src

let choiceInstruction (decl: TypeCheck.Declaration) : Instruction list =
    failwith "?"