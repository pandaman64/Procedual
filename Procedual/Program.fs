// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open Common
open Parser
open FParsec

module TC = TypeCheck

[<EntryPoint>]
let main argv = 
    let source = """
function actualsolution() : Int = 21;
function solution() : Int = 2 * actualsolution();
function main() : Int = 
    let sol : Int = solution();
    sol;
"""
    let debug = false
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
            let stmts = List.reduce List.append blocks
            let body = stmts |> List.map (sprintf "%A")
            let header = 
                List.indexed Frame.registers
                |> List.map (fun (i,t) -> sprintf "r%d: %A" i t)
                |> String.concat ", "
            System.IO.File.WriteAllLines(sprintf "%A.ir" decl.name, List.toArray (header :: body))

        let instructions = InstructionChoice.choiceInstructions decls
        for kv in instructions do
            printfn "---------------"
            printfn "%A" kv.Key
            //for inst in kv.Value do
                //printfn "%A" inst

        let decls = 
            decls
            |> List.map (fun decl -> decl.name,decl)
            |> Map.ofList
        for kv in instructions do
            let name = kv.Key
            let insts = kv.Value
            let frame = (Map.find name decls).frame

            //for inst in insts do
                //printfn "%A" inst
            let insts, igraph, allocation = GreedyRegisterAllocation.allocateRegisters frame insts
            Liveness.UndirectedGraph.check igraph
            let interference =
                let mutable adjacents = Set.empty
                let mutable colors = Map.empty
                let mutable visited = Set.empty
                
                let addAdjacents f t =
                    if f > t
                    then
                        adjacents <- Set.add (t,f) adjacents
                    else
                        adjacents <- Set.add (f,t) adjacents
                
                for node in igraph do
                    if not (visited.Contains node.value)
                    then
                        visited <- Set.add node.value visited
                        for a in !node.adjacents do
                            addAdjacents node.value a.Value.value
                        colors <- Map.add node.value (allocation.TryFind node.value) colors

                let adjacents = 
                    adjacents
                    |> Set.map (fun (f,t) -> sprintf "%A -- %A;" f t)
                    |> String.concat "\n"

                let colors =
                    let colorer t c =
                        let bucket = 
                            [
                                "cyan";
                                "brown";
                                "coral";
                                "darkgreen";
                                "green";
                                "pink";
                                "yellow";
                                "red";
                            ]
                        match c with
                        | Some(i) -> bucket.Item i
                        | None -> "gray"
                    colors
                    |> Map.map colorer
                    |> Map.toList
                    |> List.map (fun (t,c) -> 
                        match List.tryFindIndex ((=) t) Frame.registers with
                        | Some(i) -> sprintf "%A [label = \"%A a.k.a r%d\" color = %s];" t t i c
                        | None -> sprintf "%A [style=filled, color=\"%s\"];" t c
                        )
                    |> String.concat "\n"

                sprintf "graph g{\n%s\n%s\n}" adjacents colors
            System.IO.File.WriteAllText(sprintf "%A.igraph.dot" name,interference)
            let insts_virtual, insts = 
                let finder t =
                    match allocation.TryFind t with
                    | Some(i) -> i
                    | None ->
                        printfn "%A cannot be allocated" t
                        -1
                    |> sprintf "r%d"
                insts
                |> List.map (fun inst -> inst.EmitRealAssembly (sprintf "%A") debug),
                insts
                |> List.map (fun inst -> inst.EmitRealAssembly finder debug)
            System.IO.File.WriteAllLines(sprintf "%A.virtual.asm" name,insts_virtual |> List.toArray)
            System.IO.File.WriteAllLines(sprintf "%A.real.asm" name,insts |> List.toArray)
            
    | Failure(_,err,_) -> printfn "%A" err
    0 // 整数の終了コードを返します
