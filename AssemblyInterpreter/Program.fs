// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

type Environment = {
    registers: int list
    memories: Map<int,int>
    labels: Map<string,int>
    asm: string list
    pc: int
}

let rec insert i x xs =
    match i,xs with
    | 0, _ :: xs -> x :: xs
    | n, x' :: xs -> x' :: insert (n - 1) x xs
    | _, [] -> failwith "?"

let extract (op: string list) =
    let operands = (op.Item 1).Split ',' |> Array.toList
    let dst = ((operands.Item 0).TrimStart 'r') |> int
    let src = ((operands.Item 1).TrimStart 'r') |> int
    dst,src

let step (env: Environment): Environment = 
    let op = env.asm.Item env.pc
    let op = op.Split ' ' |> Array.toList
    if op.Item 0 = "MV"
    then
        let dst,src = extract op
        let registers = insert dst (env.registers.Item src) env.registers
        {
            registers = registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "ADDI"
    then
        let dst,value = extract op
        let registers = insert dst (env.registers.Item dst + value) env.registers
        {
            registers = registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "LDI"
    then
        let dst,value = extract op
        let registers = insert dst value env.registers
        {
            registers = registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "ST"
    then
        let dst,src = extract op
        let memories = Map.add (env.registers.Item dst) (env.registers.Item src) env.memories
        {
            registers = env.registers
            memories = memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "LD"
    then
        let dst,src = extract op
        let registers = insert dst (Map.find (env.registers.Item src) env.memories) env.registers
        {
            registers = registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "ADD"
    then
        let dst,src = extract op
        let registers = insert dst (env.registers.Item dst + env.registers.Item src) env.registers
        {
            registers = registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "EQ"
    then
        let dst,src = extract op
        let value = 
            if env.registers.Item dst = env.registers.Item src
            then
                1
            else
                0
        let registers = insert dst value env.registers
        {
            registers = registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    elif op.Item 0 = "J"
    then
        let dst = op.Item 1
        {
            registers = env.registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = Map.find dst env.labels
        }
    elif op.Item 0 = "JAL"
    then
        let dst = op.Item 1
        {
            registers = insert 7 env.pc env.registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = Map.find dst env.labels
        }
    elif op.Item 0 = "JR"
    then
        let dst = (op.Item 1).TrimStart 'r' |> int
        {
            registers = env.registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = dst
        }
    elif op.Item 0 = "BNZ"
    then
        let operand = (op.Item 1).Split ',' |> List.ofArray
        let cond = (operand.Item 0).TrimStart 'r' |> int
        let dst = operand.Item 1
        if env.registers.Item cond <> 0
        then
            {
                registers = env.registers
                memories = env.memories
                labels = env.labels
                asm = env.asm
                pc = Map.find dst env.labels
            }
        else
            {
                registers = env.registers
                memories = env.memories
                labels = env.labels
                asm = env.asm
                pc = env.pc + 1
            }
    elif (op.Item 0).StartsWith "L"
    then
        printfn "label"
        {
            registers = env.registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }
    else
        printfn "not implemented"
        {
            registers = env.registers
            memories = env.memories
            labels = env.labels
            asm = env.asm
            pc = env.pc + 1
        }

[<EntryPoint>]
let main argv = 
    let mutable env = 
        let path = "main.asm"
        let asm = System.IO.File.ReadAllLines(path) |> Array.toList 
        let labels =
            List.indexed asm
            |> List.where (fun (_,line) -> line.EndsWith ":")
            |> List.map (fun (i,line) -> line.TrimEnd ':',i)
            |> Map.ofList
        let registers = 
            printfn "Input initial registers:"
            (System.Console.ReadLine()).Split ' '
            |> List.ofArray
            |> List.map int
        let memories = Map.empty
        { registers = registers; memories = memories; labels = labels; asm = asm; pc = 0 }

    while true do
        printfn "%3d: %A %A: %s" env.pc env.registers env.memories (env.asm.Item env.pc)
        env <- step env
        System.Console.ReadLine() |> ignore
    0 // 整数の終了コードを返します
