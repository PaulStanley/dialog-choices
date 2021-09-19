# dialog-choices
Compile from a subset of Inkle's Ink scripting language to Dialog 

This is still work in progress.

The compiler recognises a sub-set of Inkle's Ink language (https://github.com/inkle/ink). It compiles to code which can be compiled by Dialog (https://www.linusakesson.net/dialog/). It is written in F#. Its only dependency is on FFParsec.

In terms of Ink it implements:

* Knots (but not sub-knots or named sub-parts)
* Choices (ordinary and sticky)
* Gathers
* Diverts
* Conditions for choices (using {})

It does not attempt to implement the whole thing, because in practice much of what you do in Ink natively would be done in Dialog using Dialog code.

Usage: Choices infile [outfile]
