# ulang

A small programming language with pure functional expressions, imperative statements, algebraic datatypes, and Hindley-Milner type inference.
It is implemented in itself: an interpreter and a compiler to C written in ulang, where the compiler compiles itself.
The first implementation, an interpreter in Scala 3, is on the branch `scala-baseline`.

This program sorts numbers by inserting them into a search tree:

```
data Tree[A] :=
  case Leaf
  case Node(left: Tree[A], key: A, right: Tree[A])

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
    case Leaf => list()
    case Node(l, k, r) => keys(l) ++ list(k; keys(r))
  end

// inserts the elements of xs into the search tree t, one at a time
procedure insert_all(in xs: List[Int], ref t: Tree[Int]) :=
  for x in xs do
    assign t := insert(x, t)
  end

procedure main() :=
  local t := Leaf

  call insert_all(in list(3, 1, 2, 1), ref t)
  call os_println(in show(keys(t)))

test sorting() :=
  local t := Node(Leaf, 2, Leaf)

  call   insert_all(in list(3, 1, 2), ref t)
  assert keys(t) == list(1, 2, 3)
```

## Language

- Functions are pure expressions, procedures are statements with `in`, `out`, and `ref` parameters.
- Functions and procedures are values: `lambda x => e`, whose formals may be patterns, as in `lambda n, (a, b) => n + a * b`, and `lambda in x, out y => statements end` of a type `(in A, out B)`, which sees the variables around it but assigns only its outs and refs.
- `if` always nests: an `if` after `else` is an `if` in its body, which has its own `end`.
- Datatypes with pattern matching, guards, and checks that matches are exhaustive and cases reachable.
- Named fields of constructors, read as `e.field` and updated as `e with field := a, other := b`, which are resolved by the type of `e`, and raise `MissingField` for a constructor without that field.
- Type inference with let-polymorphism, where top-level declarations carry their types.
- Exceptions of any type, raised by `raise e` in statements and expressions, and caught by `try`, which leave the variables as they were: outs and refs reach the caller only when a procedure returns, and handlers see the variables as they were before the `try`.
  Builtins raise `DivisionByZero`, `OutOfBounds`, and `NoCharacter`, whereas a failed `assert` ends the program.
- Unbounded integers, characters, which are Unicode code points, and strings of them, which files hold as UTF-8.
- Programs read their arguments with `os_args`, files with `os_read` and `os_write`, and print strings with `os_print`, all of the module `os`, where the builtin `show` gives the text of any value, which a datatype may declare for itself by a function `show`.
- The forms `list(1, 2)`, `set(a, b)`, `map(k => v)`, and `array(a, b)`, and their generators `list(for x in xs if x > 0 yield x * x)`, `set(for x in c yield f(x))`, `map(for k => v in m yield k => f(v))`, which range over lists, sets, arrays, or maps by their folds, and `==` compares their elements and entries; and the statement `for x in xs if p do … end`, which runs its body for the elements of any container.
- User-defined operators by `infix`, `prefix`, and `postfix` notations.
- Modules: `import "name.u"` makes the declarations of the module `name` of a file available, but not transitively, while every module sees the whole library without imports.
- Named tests `test name() := …`, whose parameters are reserved for later and must be empty, while programs start at a procedure without parameters, like `procedure main() := …`.

The library in `prelude/` contains the prelude with natural numbers, options, and lists, as well as maps, sets, and access to files.

## Usage

Requires a C compiler, GMP, and the Boehm garbage collector. The interpreter recurses as deeply as the programs it runs, so `ulimit -s unlimited` is advisable.

```
make                                      # builds out/ulang.bin from the sources, by the seed in bootstrap/
out/ulang.bin run file.u -- args          # checks a program and calls its procedure main
out/ulang.bin test file.u                 # runs its tests one by one
out/ulang.bin test dir                    # runs the files below dir, which must pass or fail as their first lines state
out/ulang.bin check file.u                # only checks it
out/ulang.bin compile file.u main file.c  # compiles a program to C, whose main calls the procedure main
cc -I ulang/runtime file.c ulang/runtime/ulang.c -lgmp -lgc
make test                                 # runs all tests
```

## Layout

- `ulang/` is the implementation in ulang: lexer, parser, checker, interpreter, and the compiler to C, with its runtime in `ulang/runtime/`, and the driver `ulang/ulang.u`.
- `bootstrap/ulang.c` is the seed, the C of the last compiler that compiled itself to the same C, from which `make` builds
  `bootstrap/ulang.bin`, which compiles the sources to `out/ulang.bin`. `make selfcompile` replaces the seed by the compiler
  compiled with itself, and `make bootstrap` by the compiler compiled with the interpreter.
- `prelude/` is the library.
- `tests/` are the tests, also of expected errors in `tests/errors/`.
- `vscode/` is an extension for VS Code and VSCodium, installed by `make install`.
- `TODO.md` lists the open work, and `DESIGN.md` the design notes.
- `prove/` is a prover by contextual simplification, with its tests, on terms of its own.
