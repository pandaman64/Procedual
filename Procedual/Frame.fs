module Frame

open Common

type Access = 
    InMemory of int // offset from fp
    | InRegister of Temporary.Temporary // temporary register
type Frame = { 
    name: Var;
    arguments: Access list;   
    currentOffset: int; 
}

let newFrame (name: Var) (arguments: (int * bool) list) : Frame = 
    // always spill arguments for the sake of simplicity of compiler
    let mapFolder offset (size,_) =
        let offset = offset - size
        InMemory(offset),offset
    let arguments = List.mapFold mapFolder 0 arguments |> fst
    { name = name; arguments = arguments; currentOffset = 0 }

let allocLocal (frame: Frame) (size: int) (spill: bool) =
    { name = frame.name; arguments = frame.arguments; currentOffset = frame.currentOffset + size},
    InMemory(frame.currentOffset)