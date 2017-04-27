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
value main : Int =
    let x : Int = { 
        fact(3);
        2
    };
    fact(x);
"""
    printfn "%s" source
    match run Parser.pProgram source with
    | Success(decls,_,_) -> 
        printfn "%A" decls
        let decls = AlphaTransform.transformDecls decls
        printfn "%A" decls
        let env =
            let arithmeticOpType = TC.TArrow(TC.intType,TC.TArrow(TC.intType,TC.intType))
            let relationalOpType = TC.TArrow(TC.intType,TC.TArrow(TC.intType,TC.boolType))
            let assignOpType = TC.TArrow(TC.intType,TC.TArrow(TC.intType,TC.unitType))
            [
                ExternalName(Var("+")),arithmeticOpType;
                ExternalName(Var("-")),arithmeticOpType;
                ExternalName(Var("*")),arithmeticOpType;
                ExternalName(Var("/")),arithmeticOpType;
                ExternalName(Var("=")),relationalOpType;
                ExternalName(Var("<>")),relationalOpType;
                ExternalName(Var(">=")),relationalOpType;
                ExternalName(Var(">")),relationalOpType;
                ExternalName(Var("<=")),relationalOpType;
                ExternalName(Var("<")),relationalOpType;
                ExternalName(Var(":=")),assignOpType;
            ]
            |> Map.ofList
        let decls = TypeCheck.checkDecls env decls
        printfn "%A" decls
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
