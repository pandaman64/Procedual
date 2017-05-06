﻿// F# の詳細については、http://fsharp.org を参照してください
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
    let x : Int = fib(3);
    {
        x := 4;
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

        let decls = InstructionChoice.choiceInstructions decls
        for decl in decls do
            printfn "---------------"
            printfn "%A" decl.Key
            for inst in decl.Value do
                printfn "%A" inst

        for decl in decls do
            System.IO.File.WriteAllText(sprintf "%A.dot" decl.Key,sprintf "%A" (Liveness.FlowGraph.makeGraph decl.Value))
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
