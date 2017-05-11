// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open Common
open Parser
open FParsec

module TC = TypeCheck

[<EntryPoint>]
let main argv = 
    let source = """
function fib(x: Int) : Int =
    if x = 0 then 1
    else if x = 1 then 1
    else fib(x - 1) + fib(x - 2);
function main() : Int =
    let x : Int = 1;
    {
        x := x + x;
        fib(x)
    };
"""
    printfn "%s" source
    match run Parser.pProgram source with
    | Success(decls,_,_) -> 
        printfn "%A" decls
        let decls = AlphaTransform.transformDecls decls
        printfn "%A" decls
        let env = Map.empty
        let decls = TypeCheck.checkDecls env decls
        printfn "%A" decls

        printfn "Register(RV) is %A" Frame.returnValue
        for decl in decls do
            printfn "-----------------"
            printfn "%A" decl.name
            printfn "\nbasic blocks"
            let blocks = decl.body
            for block in blocks do
                for stmt in block do
                    printfn "%A" stmt

        let instructions = InstructionChoice.choiceInstructions decls
        for kv in instructions do
            printfn "---------------"
            printfn "%A" kv.Key
            for inst in kv.Value do
                printfn "%A" inst

        (*let cfgs = Map.map (fun _ insts -> Liveness.FlowGraph.makeGraph insts) decls
        for cfg in cfgs do
            System.IO.File.WriteAllText(sprintf "%A.dot" cfg.Key,sprintf "%A" cfg.Value)

        let igraphs = Map.map (fun _ cfg -> Liveness.Intereference.analyzeIntereference' cfg) cfgs
        for igraph in igraphs do
            let mutable visited = Map.empty 
            let name = igraph.Key
            let igraph = igraph.Value

            let mutable text = []
            let rec visit (node: Liveness.Intereference.Node) =
                match visited.TryFind node.id with
                | Some(true) -> ignore "do nothing"
                | _ -> 
                    visited <- Map.add node.id true visited
                    text <- sprintf "%A;" node.value :: text 
                    for adj in !node.adjacents do
                        let adj = adj.Value
                        text <- sprintf "%A -- %A;" node.value adj.value :: text 
                        visit adj
            for node in igraph do
                visit node

            let text = List.distinct text |> String.concat "\n"
            System.IO.File.WriteAllText(sprintf "%A.igraph.dot" name,sprintf "graph G{\n%s\n}" text)

        for igraph in igraphs do
            let name = igraph.Key
            let igraph = igraph.Value

            let precolored = 
                List.zip Frame.registers (List.init 8 id)
                |> Map.ofList

            let colors = RegisterAllocation.tryAllocateRegisters igraph precolored
            printfn "%A's register allocation" name
            printfn "%A" colors*)

        let decls = 
            decls
            |> List.map (fun decl -> decl.name,decl)
            |> Map.ofList
        for kv in instructions do
            let name = kv.Key
            let insts = kv.Value
            let frame = (Map.find name decls).frame

            let insts = RegisterAllocation.allocateRegisters frame insts
            System.IO.File.WriteAllLines(sprintf "%A.asm" name,insts |> List.toArray)
            
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
