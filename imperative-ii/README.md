# Imperative II

In this language, a program is a list of functions.
The entry point of the program is the `main` function.

## Examples

- Sum of two values

```
func main() {
  x = 1;
  y = 2;
  z = x + y;
}
```

- Function call

```
func function() {
  return 1024;
}

func main() {
  x = 512;
  y = function();
  z = y - x;
}
```

- Fibonacci

```
func fibonacci(n) {
  a = 0;
  b = 1;
  while (n) {
    tmp = a + b;
    a = b;
    b = tmp;
    n = n - 1;
  }

  return a;
}

func main() {
  x = fibonacci(10);
}
```

## Usage

`stack build` builds the project.

`stack run <filename>` runs a program in `filename`.
The resulting frame of the `main` function is
printed after execution.
