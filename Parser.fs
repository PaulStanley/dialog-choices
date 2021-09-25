module Parser

open System
open FParsec
open GeneralTypes

let charToString (x: char list) = x |> List.toArray |> System.String

let escapedBrack =
    pchar '\\' .>> anyOf ['['; ']']

let isAsciiIdContinue = fun (c) -> isAsciiLetter c || isDigit c

let validIdentifier : Parser<string,unit> = 
    //    many1 (letter <|> pchar '_') |>> charToString
        identifier (IdentifierOptions(isAsciiIdContinue = isAsciiIdContinue))
        

let divertTo =
    pstring "->" >>. spaces 
    >>. validIdentifier .>> many (anyOf [' '; '\t'] <?> "space")
    >>= fun a ->
        newline <?> "end of line (identifiers must be one word)"
        >>% a

let conditional: Parser<String, unit> = 
    spaces >>. pchar '{' >>. many1 (noneOf ['}']) .>> pchar '}' |>> charToString

let isArrow = function | '>' -> true | _ -> false

let preLabel : Parser<String,unit> =
    many (escapedBrack <|> noneOf ['[']) |>> charToString

let justInLabel : Parser<String,unit> =
    many (escapedBrack <|> noneOf [']']) |>> charToString

let inLabel : Parser<String, unit> =
    pchar '[' >>. justInLabel .>> pchar ']'

let blankLine: Parser<char, unit> =
    newline >>. newline

let textNoDivert : Parser<char,unit> =
    noneOf ['-'] <|> pchar '-' .>> nextCharSatisfiesNot isArrow

let starters =
    (attempt (spaces >>. (many1 (pchar '-') .>> notFollowedBy (pchar '>') |>> charToString))) // gather
    <|>
    (attempt (spaces >>. (many1 (pchar '*') |>> charToString)))
    <|>
    (attempt (spaces >>. (many1 (pchar '+') |>> charToString)))
    <|>
    (spaces >>. pstring "===") // knot


let postLabelChar =
    noneOf ['-'; '\n'; '\r']
    <|>
    attempt (pchar '-' .>> notFollowedBy (pchar '>'))
    <|>
    attempt (newline .>> notFollowedBy starters)


let testme : Parser<char, unit>=
    newline .>> notFollowedBy (spaces >>. pchar '*')

let postLabelOK =
    many1 postLabelChar |>> charToString

let postLabel =
    postLabelOK >>=
    fun (a) ->
        (followedBy (pstring "->") >>. (divertTo |>> fun (b) -> (a, b)))
        <|>
        (notFollowedBy (pstring "->") >>. (preturn (a, "")))

let labelAndDisp prelabel inlabel postlabel : (string * string * string) =
    let plText, plDivert = postlabel
    ((prelabel + inlabel), (prelabel + plText), plDivert)

let parseFullLabel =
    pipe3
        preLabel
        inLabel
        postLabel
        labelAndDisp


let knotStart =
    pstring "===" >>. spaces >>. validIdentifier .>> many (anyOf [' '; '\t'; '=']) .>> newline 

let choiceStart = 
    spaces >>. many1 (anyOf ['*' ; '+']) 
    >>= fun (a) ->
        let isSticky = 
            a.[0] = '+'
        (attempt (spaces >>. conditional >>= 
            fun (b) ->
                parseFullLabel <?> "valid text" 
                |>> fun (c) -> (List.length a, isSticky, c, b)))
        <|>
        (attempt (spaces >>. parseFullLabel <?> "valid text"
            |>> fun (b) -> ((List.length a), isSticky, b, ""))) 
        <|>
        (spaces >>. divertTo 
            |>> fun (b) -> ((List.length a), false, ("", "", b), ""))


let gatherStart =
    spaces >>. many1 (pchar '-')
    >>= fun (a) ->
        spaces >>. postLabel <?> "valid text" .>> spaces
        |>> fun (b) -> (List.length a, b)

let knot = 
    spaces >>. getPosition >>=
        fun (posn) ->
        knotStart >>=
            fun (a) ->
                (postLabel .>> spaces <|> preturn ("", "")) |>> fun (b) -> 
                    let text, divert = b
                    let disp =
                        match text.Trim() with
                        | "" -> None
                        | x -> Some x
                    let divert =
                        match divert with
                        | "" -> None
                        | x -> Some x
                    (Knot {Name = a; Disp = disp; Divert = divert; Position = Some (int posn.Line)})

let choiceP =
    spaces >>. getPosition >>=
        fun (posn) ->
        choiceStart |>> fun (a) ->
            let depth, labelType, (label, text, divert), condition = a
            let text = text.Trim() 
            let disp = 
                match text with
                | "" -> None
                | x -> Some x
            let label =
                match label with
                | "" -> None
                | x -> Some x
            let divert =
                match divert with
                | "" -> None
                | x -> Some x
            let condition =
                match condition with
                | "" -> None
                | x -> Some x
            (Choice { Name = None; Sticky = labelType; Condition = condition; Depth = depth; Label = label; Disp = disp; Divert = divert; Position = Some (int posn.Line)})


let gather =
    spaces >>. gatherStart >>=
    fun (a) ->
        getPosition |>>
        fun (posn) -> 
            let depth, (text, divert) = a
            let text = text.Trim() 
            let disp = 
                match text with
                | "" -> None
                | x -> Some x
            let divert =
                match divert with
                | "" -> None
                | x -> Some x
            (Gather {Name = None; Disp = disp; Divert = divert; Depth = depth; Position = Some (int posn.Line)})

let kCG =
    spaces >>.
    (
    (followedBy (pchar '=') >>. knot .>> spaces <?> "knot")
    <|>
    (followedBy (pchar '*') >>. choiceP .>> spaces <?> "choice")
    <|>
    (followedBy (pchar '+') >>. choiceP .>> spaces <?> "choice")
    <|>
    (followedBy (pchar '-') >>. gather .>> spaces <?> "gather"))

let parseFile =
    knot <??> "start of knot (===)">>=
        fun (a) ->  
        many kCG
        .>> eof <|> failFatally "File ended before knot, choice, or gather was complete"
        |>> fun (b) -> List.append [a] b

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %O" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %O" errorMsg

let doParse str =
    match run parseFile str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) ->
        raise (ChoiceParseException errorMsg)
        []

let takeWhile x : Parser<string, unit> = many1 (satisfy x) |>> charToString

let pureText =
    takeWhile
        (function
        | '('
        | ')'
        | '\\' -> false
        | _ -> true)

let escaped : Parser<string, unit> =
    pchar '\\' >>. anyChar
    |>> fun c -> sprintf "\\%c" c

let interiorTextBracketed =
    many1 (pureText <|> escaped)
    |>> String.concat ""
    
let bracketed, bracketedRef = createParserForwardedToRef ()
    
let interiorBracketed =
    choice [ pstring "()"
             bracketed |>> (fun a -> sprintf "(%s)" a)
             interiorTextBracketed ]
    
do
    bracketedRef
    := pchar '(' >>. many interiorBracketed
        .>> pchar ')'
        |>> String.concat ""

let bracketedAndNot =
    many1 (interiorTextBracketed <|> (bracketed |>> fun (a) -> sprintf "(%s)" a))

let wordByWord strList =
    let rec aux acc consume =
        match consume with
        | [] -> List.rev acc
        | h::t when String.length h > 0 && h.[0] = '(' ->
            aux (h::acc) t
        | h:: t ->
            let splitH = 
                h.Split [|' '; '\n'; '\r'; '\t'|]
                |> List.ofArray
                |> List.filter
                    (fun (a) -> a <> "")
                |> List.rev
            aux (List.append splitH acc) t
    aux [] strList


let preparedForOutput str =
    match run bracketedAndNot str with
    | Success(result, _, _) -> wordByWord result
    | Failure(errormsg, _, _) -> 
        printfn "Failed to reconstruct text %s" errormsg
        []
    