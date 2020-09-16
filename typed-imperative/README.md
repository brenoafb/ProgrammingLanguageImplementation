# Typed Imperative

In this language, a program is a list of functions.

There are three types:
- `int`
- `bool`
- `string`

The entry point of the program is the `main` function,
which should return `void`.

Programs are typechecked before they are run.
Execution without typechecking is possible,
but might stop abruptly if type errors occur.

## Usage

`stack build` builds the project.

`stack run <filename>` runs the program in `filename`.
The resulting frame of the `main` function is
printed after execution.


`stack run -- -t <filename>` typechecks the program in `filename`.
It finishes silently if there are no type errors, otherwise
the error is printed.


`stack run -- -p <filename>` pretty prints the AST of the program in `filename`.
The [`pretty-simple`](https://hackage.haskell.org/package/pretty-simple) library
is used for pretty printing.

## Examples

There are more examples in the `examples` directory.

- Sum of two values

```
func main() : void {
  int x;
  int y;
  int z;

  x = 1;
  y = 2;
  z = x + y;
}
```

- Function call

```
func sum(int x, int y) : int {
  return x + y;
}

func main() : void {
  int x;
  int y;
  int z;

  x = 10;
  y = 20;
  z = sum(x, y);
}
```

- Fibonacci

```
func fib(int n) : int {
  int x;
  int y;
  int tmp;

  x = 0;
  y = 1;

  while (n != 0) {
    tmp = x;
    x = y;
    y = tmp + y;
    n = n - 1;
  }

  return x;
}

func main() : void {
  int n;
  int f;

  n = 10;
  f = fib(n);
}
```
