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

let displayPart (n: Node) =
    match n.Display with
    | None -> ""
    | Some x -> "(display *)\n" + (wordWrap 4 70 x)

let divertPart (n: Node) =
    match n.Divert with
    | None -> ""
    | Some "END" -> "(terminating *)\n"
    | Some x -> sprintf "(* flows to #%s)\n" x

let oneChoice (n: Node) =
    match n.Condition with
    | None -> sprintf "(* offers %s)\n" (namePart n)
    | Some x when x.[0] = '(' || x.[0] = '~' ->
        sprintf "(* offers %s)\n%s\n" (namePart n) (wordWrap 4 70 x)
    | Some x ->
        sprintf "(* offers %s)\n    (#%s is exposed)\n" (namePart n) x

let choicesFor n =
    let rec aux consume acc =
        match consume with
        | [] -> List.rev acc
        | h::t when h.Kind = ChoiceNode ->
            aux t (oneChoice h :: acc)
        | _::t -> aux t acc
    aux n.Children [] |> String.concat ""

let labelFor (n: Node) =
    match n.Label with
    | None -> ""
    | Some x -> sprintf ("(label *)\n%s") (wordWrap 4 70 x)

let isSticky (n: Node) =
     match n.Sticky with
        | false -> ""
        | true -> "(sticky *)\n" 

let outputKnot n =
    let name = namePart n
    let display = displayPart n
    let divert = divertPart n
    let choices = choicesFor n
    let label = labelFor n
    let sticky = isSticky n
    sprintf "%s\n%s%s%s%s%s" name display label divert choices sticky

let warnLooseEnd n =
    match n.Kind with
    | GatherNode when n.Divert = None && n.Children = [] ->
        printfn "WARNING: Loose end in node output as %s?" (namePart n)
    | KnotNode when n.Divert = None && n.Children = [] ->
        printfn "WARNING: Loose end in node output as %s?" (namePart n)
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
        printfn "WARNING: External divert to '%s' in node output as %s"
            x (namePart n)
  





    




let outputNode n =
    match n.Kind with
    | KnotNode -> outputKnot n
    | GatherNode -> outputKnot n
    | ChoiceNode -> outputKnot n