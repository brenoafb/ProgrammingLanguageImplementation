# Imperative I

In this language, a program is a statement.
This includes a block statement, which is a
sequence of statement enclosed with braces.

The language only supports integers.

Types of statement include assignment, if-else,
and while.

## Examples

- Sum of two values

```
{
  x = 1;
  y = 2;
  z = x + y;
}
```

- Sum and multiplication

```
{
  x = 1;
  y = 2;
  z = x + y;
  w = x * y * z;
}
```
- If statement

```
{
  x = 0;
  y = 1;
  if (x) {
    z = 0;
  } else {
    z = 1;
  }
}
```

- Fibonacci function

```
{
  x = 0;
  y = 1;
  c = 10;
  while (c) {
    tmp = x + y;
    x = y;
    y = tmp;
    c = c - 1;
  }
}
```

## Usage

Use `stack build` to build.

`stack run -- <filename>` Will execute the program
in interpreted mode and output the resulting environment
after execution.

`stack run -- -c <filename>` runs the compiled option.
The input program will be compiled and the machine code will
be printed.
The state of the virtual machine after execution will also
be shown.


