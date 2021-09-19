module GeneralTypes

type Choice = string Option * string

type Kind = KnotNode | ChoiceNode | GatherNode

type Node = {
    Name: string Option;
    Children: Node list;
    Label: string Option;
    Condition: string Option;
    Divert: string Option;
    Display: string Option;
    Terminating: bool;
    Sticky: bool;
    Kind: Kind
    }

type KnotNode = {
    Name: string;
    Disp: string option
    Divert: string option
}

type ChoiceNode = {
    Name: string option;
    Disp: string option;
    Label: string option;
    Divert: string option;
    Depth: int
    Condition: string option;
    Sticky: bool;
}

type GatherNode = {
    Depth: int;
    Name: string option;
    Disp: string option;
    Divert: string option
}

type Widget =
    | Knot of KnotNode
    | Gather of GatherNode
    | Choice of ChoiceNode