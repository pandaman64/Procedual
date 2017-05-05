// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open Common
open Parser
open FParsec

module TC = TypeCheck

[<EntryPoint>]
let main argv = 
    let source = """
function fact(x: Int) : Int =
    if x = 0 then 1
    else x * fact(x - 1);
function main() : Int =
    let x : Int = fact(3);
    {
        x := 4;
        fact(x)
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
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
