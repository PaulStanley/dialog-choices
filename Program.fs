open System
open GeneralTypes

let printOption thing = function
    | None -> ""
    | Some str -> sprintf "\n(%s)\n %s" thing str

let printDivert = function
    | None -> ""
    | Some str -> sprintf "\n(* flows to #%s)" str

let printChoice (condition, choice) =
    match condition with
    | None -> sprintf "\n(* offers #%s)" choice
    | Some s -> sprintf "\n(* offers #%s)\n (%s)" choice s

let printChoices choices = 
    let rec asList = 
        function
        | [] -> []
        | c::cs -> (printChoice c) :: asList cs
    in asList choices |> String.concat "\n" 

let printTerminating = function
    | false -> ""
    | true -> "\n(terminating *)"

let printSticky = function
    | false -> ""
    | true -> "\n(sticky *)"

(* let rec printNode node = *)
(*     let name = sprintf "#%s" node.name
    let label = printOption "label *" node.label
    let disp = printOption "disp *" node.display
    let choices = printChoices node.choices
    let divert = printDivert node.divert
    let sticky = printSticky node.sticky
    let terminating = printTerminating node.terminating
    let kids = printSubnodes node.children
    sprintf "%s%s%s%s%s%s%s\n\n%s" name label disp choices divert sticky terminating kids

and printSubnodes nodes =
        let rec asList = function 
            | [] -> []
            | n::ns -> (printNode n) :: asList ns
        in asList nodes |> String.concat ""
 *)
    // Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        failwith "No file name. Usage: Choice infile [outfile]"
  
    let inputFile = argv.[0]

    let baseFile = System.IO.Path.GetFileNameWithoutExtension(inputFile)

    let outputFile = 
        if Array.length argv > 1 then
            argv.[1]
        else
            baseFile + ".dg"

    let parsed = System.IO.File.ReadAllText(inputFile) |> Parser.doParse
    
    let thisTree = Tree.constructTree parsed []
    let thisTree = Tree.applyNames "--" 1 thisTree []
    let thisTree = Tree.resolveDiverts [] [] thisTree
    Tree.outputTree Output.warnLooseEnd thisTree |> ignore
    let catalogue = Output.catalogueNodes thisTree
    Tree.outputTree (Output.checkNode catalogue) thisTree |> ignore
    let outputData = 
        Tree.outputTree Output.outputKnot thisTree
        |> String.concat "\n"
    let tag = sprintf "%%%% Automatically generated from %s\n" inputFile
    let outputData = tag + outputData + "\n%% END OF GENERATED OUTPUT"
    System.IO.File.WriteAllText(outputFile, outputData)
    0
