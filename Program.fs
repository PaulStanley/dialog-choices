open System
open GeneralTypes
open Argu

type CliArguments =
    | [<MainCommand; ExactlyOnce; Last>] Input_File of path:string
    | [<AltCommandLine("-o")>] Output_File of path:string
    | [<AltCommandLine("-i")>] Indent of int
    | [<AltCommandLine("-w")>] Wrap of int
    | [<AltCommandLine("-f")>] Force of bool

    interface IArgParserTemplate with

        member this.Usage =
            match this with
            | Input_File  _ -> "input file"
            | Output_File _ -> "output file"
            | Indent _ -> "spaces to indent"
            | Wrap _ -> "line to wrap at"
            | Force _ -> "force overwrite of dangerous filename"

let parseArgs args =
    let argParser = ArgumentParser.Create<CliArguments>(programName="Choices")

    let returnValue = 
        try 
            let results = argParser.ParseCommandLine args
            let inputFile = results.GetResult Input_File
            let baseName = System.IO.Path.GetFileNameWithoutExtension(inputFile)
        
            let outputFile = 
                match results.TryGetResult Output_File with
                | None -> baseName + ".dg"
                | Some x -> x
        
            let indentAt =
                match results.TryGetResult Indent with
                | None -> 4
                | Some x -> x
        
            let wrapAt =
                match results.TryGetResult Wrap with
                | None -> 70
                | Some x -> x

            let forceOverwrite =
                match results.TryGetResult Force with
                | Some true -> true
                | _ -> false

            Some (inputFile, outputFile, indentAt, wrapAt, forceOverwrite)
        
        with
        | _ -> eprintfn "%s" (argParser.PrintUsage())
               None

    returnValue
            

[<EntryPoint>]
let main argv =

    let returnValue =
        try

            let inputFile, outputFile, indentAt, wrapAt, forceOverwrite =
                match parseArgs argv with
                | None -> failwith "Invalid usage" 
                | Some (i, o, ia, wa, fo) -> i, o, ia, wa, fo

            let parsed = System.IO.File.ReadAllText(inputFile) |> Parser.doParse
    
            let thisTree = Tree.constructTree parsed []
            let thisTree = Tree.applyNames "--" 1 thisTree []
            let thisTree = Tree.addDiverts [] thisTree thisTree
            Tree.outputTree Output.warnLooseEnd thisTree |> ignore
            let catalogue = Output.catalogueNodes thisTree
            Tree.outputTree (Output.checkNode catalogue) thisTree |> ignore
            let outputData = 
                Tree.outputTree (Output.outputKnot indentAt wrapAt) thisTree
                |> String.concat "\n"
            let tag = sprintf "%%%% Automatically generated from %s\n" inputFile
            let outputData = tag + outputData + "\n%% END OF GENERATED OUTPUT"
            if (System.IO.Path.GetFileNameWithoutExtension(inputFile) <> System.IO.Path.GetFileNameWithoutExtension(outputFile)) && IO.File.Exists(outputFile) && (not forceOverwrite) then
                raise ChoiceOverwriteException
                1
            else 
                System.IO.File.WriteAllText(outputFile, outputData)
                0
        with
        | ChoiceParseException a ->
                eprintfn "%s" a
                1
        | ChoiceOverwriteException ->
                eprintfn "%s" "ERROR: Attempt to overwrite existing file with different base name\n(if you mean to do that, run with --force=true)"
                1
        | :? IO.FileNotFoundException as x ->
                eprintfn "%s" x.Message
                1
        | x ->
                if x.Message = "Invalid usage" then 1 else raise x
            

    returnValue
