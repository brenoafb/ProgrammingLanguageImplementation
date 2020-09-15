# Let Expressions

This language handles integer arithmetic expressions
with let expressions.

## Examples

```
> 1 + 2 + 3
6
> let x = 1 in x + 1
2
> let x = 1, y = 2 in x + y
3
```

## Usage

Use `stack build` to build.

`stack run` opens a repl. Type the expression and press enter to get the result.

`stack run -- -c` runs the compiled option. Upon entering an expression,
you will get the machine code and afterwards the machine state
after execution.

`stack run -- -c <filename>` will compile the program in `<filename>`
And print the machine code and machine state after execution.

