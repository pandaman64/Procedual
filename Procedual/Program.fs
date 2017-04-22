// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open Common
open Parser
open FParsec

[<EntryPoint>]
let main argv = 
    let source = """
function fact(x: Int) =
    if x = 0 then 1
    else x * fact(x - 1);
value main : Int =
    let x : Int = 2;
    fact(x);
"""
    printfn "%s" source
    match run Parser.pProgram source with
    | Success(decls,_,_) -> 
        printfn "%A" decls
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
