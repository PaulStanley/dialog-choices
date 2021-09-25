module Output

open System
open GeneralTypes

let addWord lastWord lastLength thisWord indent goal built =
    match lastWord with
    | s when String.length s = 0 -> // first
        let newBuilt = built + indent + thisWord
        let newLength = (String.length indent) + (String.length) thisWord
        (newBuilt, newLength)
    | _ -> // lastWord was a word
        let padding = 
            if thisWord.[0] = '(' && lastWord.[0] = '(' then
                ""
            else if lastWord = "~" then
                ""
            else
                " "
        if lastLength + (String.length padding) + (String.length thisWord) <= goal then
            let newBuilt = built + padding + thisWord
            let newLength = lastLength + (String.length padding) + (String.length thisWord)
            (newBuilt, newLength)
        else
            let newBuilt = built + "\n" + indent + thisWord
            let newLength = (String.length indent) + (String.length thisWord)
            (newBuilt, newLength)

let wordWrap indent lineLength str =
    let indent = sprintf "%*s" indent " "
    let stringParsed = Parser.preparedForOutput str
    let rec aux lastW lastL build consume =
        match consume with
        | [] -> build + "\n"
        | h::t ->
            let newBuild, newLength = 
                addWord lastW lastL h indent lineLength build
            aux h newLength newBuild t
    aux "" 0 "" stringParsed

let namePart (n: Node) =
    match n.Name with
    | Some x -> "#" + x
    | None -> 
        printfn "Warning: Unnamed node"
        "#unnamed"

let displayPart (n: Node) indentAt wrapAt =
    match n.Display with
    | None -> ""
    | Some x -> "(display *)\n" + (wordWrap indentAt wrapAt x)

let divertPart (n: Node) =
    match n.Divert with
    | None -> ""
    | Some "END" -> "(terminating *)\n"
    | Some x -> sprintf "(* flows to #%s)\n" x

let oneChoice (n: Node) indentAt wrapAt =
    match n.Condition with
    | None -> sprintf "(* offers %s)\n" (namePart n)
    | Some x when x.[0] = '(' || x.[0] = '~' ->
        sprintf "(* offers %s)\n%s\n" (namePart n) (wordWrap indentAt wrapAt x)
    | Some x ->
        sprintf "(* offers %s)\n%*s(#%s is exposed)\n" (namePart n) indentAt " " x

let defaultChoice (n: Node) l indentAt =
    let rec getName (x: string list) = 
        match x with
        | [] -> []
        | (h::t) ->
            let choiceName = System.Text.RegularExpressions.Regex.Match(h, "(#\w+)")
            let fullChoice = sprintf "%*s(%s is exposed)" indentAt " " choiceName.Value
            fullChoice::(getName t)
    let otherChoices = getName l |> String.concat "\n"
    sprintf "(* flows to %s)\n%s\n" (namePart n) otherChoices

let choicesFor n indentAt wrapAt=
    let rec aux consume acc =
        match consume with
        | [] -> List.rev acc
        | h::t when h.Kind = ChoiceNode && h.Label = None && h.Display = None ->
            aux t ((defaultChoice h acc indentAt)::acc)
        | h::t when h.Kind = ChoiceNode ->
            aux t (oneChoice h indentAt wrapAt :: acc)
        | _::t -> aux t acc
    aux n.Children [] |> String.concat ""

let labelFor (n: Node) indentAt wrapAt =
    match n.Label with
    | None -> ""
    | Some x -> sprintf ("(label *)\n%s") (wordWrap indentAt wrapAt x)

let isSticky (n: Node) =
     match n.Sticky with
        | false -> ""
        | true -> "(sticky *)\n" 

let positionPart (n: Node) =
    match n.Position with
    | None -> ""
    | Some x -> sprintf "%%%% source line %d\n" x

let posPart (n: Node) =
    match n.Position with
    | None -> "[ERROR! No position recorded]"
    | Some x -> sprintf "%d" x

let outputKnot indentAt wrapAt n =
    let position = positionPart n
    let name = namePart n
    let display = displayPart n indentAt wrapAt
    let divert = divertPart n
    let choices = choicesFor n indentAt wrapAt
    let label = labelFor n indentAt wrapAt
    let sticky = isSticky n
    sprintf "%s%s\n%s%s%s%s%s" position name display label divert choices sticky

let warnLooseEnd n =
    match n.Kind with
    | GatherNode when n.Divert = None && n.Children = [] ->
        eprintfn "WARNING: Loose end in node output as %s (node begins at source line %s)?" (namePart n) (posPart n)
    | KnotNode when n.Divert = None && n.Children = [] ->
        eprintfn "WARNING: Loose end in node output as %s (node begins at source line %s)?" (namePart n) (posPart n)
    | _ -> ()

let catalogueNode (n: Node) =
    match n.Name with 
    | None -> ""
    | Some x -> x

let catalogueNodes t =
    Tree.outputTree catalogueNode t 
    |> List.filter
        (fun (a) -> a <> "")

let checkNode catalogue (n: Node) =
    match n.Divert with
    | None -> ()
    | Some x when List.contains x catalogue -> ()
    | Some x when x = "END" -> ()
    | Some x ->
        eprintfn "WARNING: External divert to '%s' in node output as %s (node begins at source line %s)"
            x (namePart n) (posPart n)
 
