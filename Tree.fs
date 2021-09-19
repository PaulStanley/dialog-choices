module Tree

open System

open GeneralTypes

let depthOf node =
    match node with
    | Knot _ -> 0
    | Choice x -> x.Depth
    | Gather x -> x.Depth

let rec constructSubTree depth consume acc =
    match consume with
        | [] -> (List.rev acc), []
        | h::_ when depthOf h < depth ->
            (List.rev acc, consume)
        | h::t when depthOf h > depth -> 
            constructSubTree depth t acc
        | h::t ->
            match h with
            | Choice x ->
                let newRoot =
                    {
                        Name = x.Name;
                        Label = x.Label;
                        Display = x.Disp;
                        Children = [];
                        Condition = x.Condition;
                        Terminating = false;
                        Sticky = x.Sticky;
                        Divert = x.Divert;
                        Kind = ChoiceNode
                    }
                let kids, rest = constructSubTree (depth + 1) t []
                constructSubTree depth rest ({newRoot with Children = kids}::acc)
            | Gather x ->
                let newRoot = 
                    {
                        Name = x.Name;
                        Label = None;
                        Display = x.Disp;
                        Children = [];
                        Terminating = false;
                        Sticky = false;
                        Condition = None;
                        Divert = x.Divert;
                        Kind = GatherNode
                    }
                let kids, rest = constructSubTree (depth) t []
                constructSubTree depth rest ({newRoot with Children = kids}::acc)
            | Knot _ -> (List.rev acc), consume
    


let rec constructTree consume acc =
    match consume with
    | [] -> List.rev acc
    | h::t ->
        match h with
        | Knot x -> 
            let newRoot = 
                {
                    Name = Some x.Name;
                    Label = None;
                    Divert = x.Divert;
                    Children = []
                    Display = x.Disp
                    Terminating = false; //WRONG -- need to look at divert
                    Sticky = false;
                    Condition = None;
                    Kind = KnotNode
                }
            let kids, rest = constructSubTree 1 t []
            constructTree rest ({ newRoot with Children = kids }::acc)
        | _ -> constructTree t acc

let rec applyNames (previous: string) mastercount (consume: Node list) (acc: Node list) =
    let applyName prev count (node: Node) =
        match node.Name with
        | Some s ->
            let kids = applyNames s 1 node.Children []
            ({ node with Children = kids}, count)
        | None ->
            let name = prev + "_" + (string count)
            let kids = applyNames name 1 node.Children []
            ({ node with Name = (Some name); Children = kids }, count + 1)
    match consume with
    | [] -> List.rev acc
    | h::t ->
        let thisNode, newCount = applyName previous mastercount h
        applyNames previous newCount t (thisNode::acc)

let nextDivert inner outer =
    let rec aux = function
    | [] -> None
    | h::_ when h.Kind = GatherNode -> 
        match h.Name with
        | Some x -> Some x
        | None -> None
    | _::t -> aux t
    match aux inner with
        | None -> aux outer
        | x -> x

let rec resolveDiverts acc outer consume =
    let resolveDivert n =
        let kids =
            resolveDiverts [] consume n.Children
        match n.Kind with
        | ChoiceNode ->
            match n.Divert with
            | Some _ -> {n with Children = kids}
            | None -> {n with Divert = nextDivert kids consume; Children = kids}
        | _ -> {n with Children = kids}
    match consume with
    | [] -> List.rev acc
    | h::t -> resolveDiverts (resolveDivert h::acc) consume t

let rec outputTree fn tree =
    match tree with
    | [] -> []
    | h::t ->
        let kidsDone = outputTree fn h.Children
        let thisDone = fn h
        let restDone = outputTree fn t
        List.append (List.append [thisDone] kidsDone) restDone

let pname (n: Node) =
    match n.Name with
    | Some x -> x
    | None -> "un-named"