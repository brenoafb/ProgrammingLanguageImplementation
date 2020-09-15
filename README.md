# Programming Language Implementation in Haskell

This repository contains implementations of programming
languages of increasing complexity in Haskell.

Every langauge implementation is presented as a [Stack](https://haskellstack.org) project.
Front-ends are implemented using Parsec.

The idea is also to iteratively use more involved
Haskell features.
These features, while not mandatory, greatly aid in
easing the code complexity when working with
higher level constructs.

The languages implemented are (in ascending complexity)

1. `arithmetic-expressions` - A calculator
2. `let-expressions` - A caltulator with let expressions
3. `imperative` - Program is a sequence of integer operations, including variable assignment
4. `imperative-ii` - Adds functions to `imperative`
5. `typed-imperative` - Imperative language with `int`, `bool`, `string`. Includes a typechecker.

