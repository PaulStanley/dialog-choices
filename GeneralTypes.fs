module GeneralTypes

exception ChoiceParseException of string
exception ChoiceOverwriteException

type Choice = string Option * string

type Kind = KnotNode | ChoiceNode | GatherNode

type Node = {
    Name: string Option;
    Children: Node list;
    Label: string Option;
    Condition: string Option;
    Position: int Option;
    Divert: string Option;
    Display: string Option;
    Terminating: bool;
    Sticky: bool;
    Kind: Kind
    }

type KnotNode = {
    Name: string;
    Disp: string Option;
    Position: int Option;
    Divert: string Option
}

type ChoiceNode = {
    Name: string Option;
    Disp: string Option;
    Label: string Option;
    Divert: string Option;
    Depth: int;
    Condition: string Option;
    Position: int Option;
    Sticky: bool;
}

type GatherNode = {
    Depth: int;
    Name: string Option;
    Disp: string Option;
    Position: int Option;
    Divert: string Option
}

type Widget =
    | Knot of KnotNode
    | Gather of GatherNode
    | Choice of ChoiceNode