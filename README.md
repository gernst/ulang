# ulang

A small programming language with pure functional expressions, imperative statements, algebraic datatypes, and Hindley-Milner type inference.
It is implemented in itself: an interpreter and a compiler to C written in ulang, where the compiler compiles itself.
The first implementation, an interpreter in Scala 3, is on the branch `scala-baseline`.

This program sorts numbers by inserting them into a search tree:

```
import os

data Tree[A] :=
  | Leaf
  | Node(left: Tree[A], key: A, right: Tree[A])

function insert(k: Int, t: Tree[Int]): Tree[Int] :=
  match t
    case Leaf => Node(Leaf, k, Leaf)
    case Node(l, j, r) =>
      if k < j then t with left := insert(k, l)
      else if j < k then t with right := insert(k, r)
      else t
  end

function keys[A](t: Tree[A]): List[A] :=
  match t
    case Leaf => []
    case Node(l, k, r) => keys(l) ++ [k; keys(r)]
  end

// inserts the elements of xs into the search tree t, one at a time
procedure insert_all(in xs: List[Int], ref t: Tree[Int]) :=
  local rest := xs

  while rest != []
    match rest
      case [x; more] =>
        assign t := insert(x, t), rest := more
      case [] =>
        skip
    end
  end

procedure main() :=
  local t := Leaf

  call insert_all(in [3, 1, 2, 1], ref t)
  call os_show(in keys(t))

test sorting() :=
  local t := Node(Leaf, 2, Leaf)

  call   insert_all(in [3, 1, 2], ref t)
  assert keys(t) == [1, 2, 3]
```

## Language

- Functions are pure expressions, procedures are statements with `in`, `out`, and `ref` parameters.
- Functions and procedures are values: `lambda x => e`, and `lambda in x, out y => statements end` of a type `(in A, out B)`, which sees the variables around it but assigns only its outs and refs.
- Datatypes with pattern matching, guards, and checks that matches are exhaustive and cases reachable.
- Named fields of constructors, read as `e.field` and updated as `e with field := a, other := b`, which are resolved by the type of `e`, and raise `MissingField` for a constructor without that field.
- Type inference with let-polymorphism, where top-level declarations carry their types.
- Exceptions of any type, raised by `raise e` in statements and expressions, and caught by `try`.
  Builtins raise `DivisionByZero`, `NoIndex`, and `NoCharacter`, whereas a failed `assert` ends the program.
- Unbounded integers, characters, and strings.
- Programs read their arguments with `os_args`, files with `os_read` and `os_write`, and print strings with `os_print` and any value with `os_show`, all of the module `os`.
- Literals and comprehensions of lists `[x * x for x in xs if x > 0]`, sets `{a, b}` and `{f(x) for x in s}`, and maps `{k => v}` and `{k => f(v) for k => v in m}`, where sets and maps need `import set` and `import map`, and `==` compares the shapes of their trees, not only their elements.
- User-defined operators by `infix`, `prefix`, and `postfix` notations.
- Modules: `import name` makes the declarations of a file available, but not transitively.
- Named tests `test name() := …`, whose parameters are reserved for later and must be empty, while programs start at a procedure without parameters, like `procedure main() := …`.

The library in `prelude/` contains the prelude with natural numbers, options, and lists, as well as maps, sets, and access to files.

## Usage

Requires a C compiler, GMP, and the Boehm garbage collector. The interpreter recurses as deeply as the programs it runs, so `ulimit -s unlimited` is advisable.

```
make                                      # builds ./ulang.bin from bootstrap/ulang.c
./ulang.bin run file.u -- args            # checks a program and calls its procedure main
./ulang.bin test file.u                   # runs its tests one by one
./ulang.bin test dir                      # runs the files below dir, which must pass or fail as their first lines state
./ulang.bin check file.u                  # only checks it
./ulang.bin compile file.u main file.c    # compiles a program to C, whose main calls the procedure main
cc -I self/runtime file.c self/runtime/ulang.c -lgmp -lgc
make test                                 # runs all tests
```

## Layout

- `self/` is the implementation in ulang: lexer, parser, checker, interpreter, and the compiler to C, with its runtime in `self/runtime/`, and the driver `self/ulang.u`.
- `bootstrap/ulang.c` is the C of the last compiler that compiled itself to the same C, from which `make` builds `ulang.bin`.
  `make selfcompile` rebuilds the compiler with itself, and `make bootstrap` with the interpreter.
- `prelude/` is the library.
- `tests/` are the tests, also of expected errors in `tests/errors/`.
- `vscode/` is an extension for VS Code and VSCodium, installed by `make install`.
- `TODO.md` lists the open work, and `DESIGN.md` the design notes and the design of verification.
