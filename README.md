# dialog-choices

## Summary
Compile from a subset of Inkle's Ink scripting language to Dialog. This is (obviously) pointless if you are writing a *purely* choice-based game: you'd be far better probably using Ink directly. But if you are writing a "mixed" parser/choice game with substantial choice-based segments, for instance for conversation, it may help. 

This is still work in progress.

The compiler recognises a sub-set of Inkle's Ink language (https://github.com/inkle/ink). It compiles to code which can be compiled by Dialog (https://www.linusakesson.net/dialog/). It is written in F#. Its only dependency is on FFParsec.

In terms of Ink it implements:

* Knots (but not sub-knots or named sub-parts)
* Choices (ordinary and sticky)
* Gathers
* Diverts
* Conditions for choices (using {})
* Default choices

It does not attempt to implement the whole thing, because in practice much of what you do in Ink natively would be done in Dialog using Dialog code.

Usage: Choices infile [outfile]

(In practical use, best really to construct suitable Makefiles.)

##  Notes

### Knots

A *knot* is the basic unit. A knot consists of a named block of text
with gathers or choices (described below).

A knot begins with `===` then the knot name. The knot name can contain only numbers and letters: you **cannot** use `_` in a knot name: better use PascalCase instead.

    === KnotName

You may (but do not have to) put equals signs after the knot as well.

    === KnotName ===

Nothing else should appear on the line.

Ink has the notion of sub-knots. Dialog-Choice does not include these.

Immediately after the knot name you place some (optional) text which
will be printed as soon as the knot is entered.

    === Start

    Uncle Joe sits down, lights his pipe slowly, and sighs.
    (par)
    "It's been a strange old day," he says.

Notes:

1. Indentation is not important. It's up to you how you indent the 
   file.

2. You can use Dialog expressions (here, `(par)`) to format text, or 
   indeed for any other purpose (setting variables, etc.)

### Choices

Mostly you will not just want to announce something, but to offer 
some choices.

A choice begins with one or more asterisks `*` or plus signs `+`. The 
difference is that choices given with asterisks are "one-time", 
whereas choices given with `+` are "sticky" and may be offered 
repeatedly.

In Dialog a choice is presented with a label (the prompt to select
it, and then (when chosen) triggers some text (the `(disp $)` in 
Dialog). In Dialog-Choice, the label and display text are constructed
as follows:

* Anything up to and including the first set of square brackets goes
  in *both* label *and* display.
* Anything in the first set of square brackets goes in the *label*
  only.
* Anything *after* the first set of square brackets goes in the
  display only.

Some examples may make that clearer

    * "Bonkers["]," he says.

Produces:

* A *label* of `"Bonkers"`
* Display text of `"Bonkers," he says.`

Whereas

    * "Bonkers!" [] he says.

Produces

* A *label* of `"Bonkers!"`
* Display text of `"Bonkers!" he says.`

While

    * [Tell him he's bonkers] "You are bonkers!" you say.

Produces

* A label of `Tell him he's bonkers`
* Text of `"You are bonkers!" you say`

## Diverts

After a choice (or after the text of knots, or gathers) you can
*divert* to another knot (or, for that matter, back to the same one):

    === Guess

    "Can you guess the number?" he says. -> DoGuess

    === DoGuess

    * "Is it 4?["]," you ask.
      (par)
      "Too low!" -> DoGuess
    * "Is it 7?["]," you ask.
      (par)
      "Too high!" -> DoGuess
    * "Is it ?["]," you guess.
      (par)
      "Exactly right," he says. -> END

That demonstrates another feature: the special divert instruction
`END` which tells the system to treat this as the end of the
choice sequence.

(If you leave "loose ends" where the conversation runs out 
unexpectedly, the compiler will warn about them. But you may have
your reasons, so it will still compile.)

## Conditions

You can put a condition in a choice, in which case the choice
will only be displayed if the condition is met. Place the
condition in `{}` at the start of the choice.

    * {(#bedroom is visited)} "The bedroom seems nice["],"
      you say."
      (par)
      "I thought it was a bit frowsy," she says.

There is one special short-form. If you just put a single word
in the curly brackets, with no parentheses, this will be assumed
to be the condition that the knot with that name has been
exposed. So

    * {refusal} "Why refuse?" [] you ask.

Will translate in dialog code to to

    ($ offers #[choice])
        (#refusal is exposed)

## Gathers

A gather is a bit like an anonymous knot. It is produced by beginning 
a line with a hyphen `-`:

    === KnotName

    The knot begins.

    * [Choice 1] You choose 1.
    * [Choice 2] You choose 2.
    * [Choice 3] You choose 3. -> Divert

    - And get to the gather.

    === Divert

    This is the divert. -> END

A gather works as follows: if a previous choice does not explicitly
divert somewhere, then the flow will "drop down" to the next gather.
So in this example, the selection of choices 1 or 2 will lead 
immediately (after their own text has been produced) to the gather.
On the other hand, since the third choice has an explicit diversion,
it will go there.

Note: I have not yet decided whether gathers should divert to 
gathers. At the moment they do not.

## Default choices

If you include a choice without either label or text, but just with a 
divert, it will be used as a "default choice". If all the other 
choices have been exhausted, the node will flow to the specified knot.

    === UselessShop
    "What can I get you?" asks the shop assistant.

    * [Bread] "But we have no bread," he says. -> UselessShop
    * [Butter] "No butter here, try the dairy!" he says. -> UselessShop
    * [Jam] "We don't sell jam," he announces. -> UselessShop
    * -> WellWhat

    === WellWhat
    "Well, what do you sell?" you ask.
    (par)
    "Dreams!" he says, "Dreams of bread, butter, and jam."