# ulang

A small programming language with pure functional expressions, imperative statements, algebraic datatypes, and Hindley-Milner type inference.
It comes with an interpreter in Scala 3 and a compiler to C that is written in ulang itself and compiles itself.

```
data Tree[A] :=
  | Leaf
  | Node(left: Tree[A], key: A, right: Tree[A])

function size[A](t: Tree[A]): Int :=
  match t
    case Leaf => 0
    case Node(l, _, r) => size(l) + 1 + size(r)
  end

function smallest(t: Tree[Int]): Int :=
  match t.left
    case Leaf => t.key
    case _ => smallest(t.left)
  end

procedure count(in t: Tree[Int], out n: Int) :=
  local k := size(t)

  assign n := k

test sizes :=
  local n: Int

  call   count(in Node(Leaf, 1, Node(Leaf, 2, Leaf)), out n)
  assert n == 2 and smallest(Node(Node(Leaf, 0, Leaf), 1, Leaf)) == 0
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
- Literals and comprehensions of lists `[x * x for x in xs if x > 0]`, sets `{a, b}` and `{f(x) for x in s}`, and maps `{k => v}` and `{k => f(v) for k => v in m}`, where sets and maps need `import set` and `import map`, and `==` compares the shapes of their trees, not only their elements.
- User-defined operators by `infix`, `prefix`, and `postfix` notations.
- Modules: `import name` makes the declarations of a file available, but not transitively.
- Named tests `test name := …`, while programs start at a procedure without parameters, like `procedure main() := …`.

The library in `ulang/resources/` contains the prelude with natural numbers, options, and lists, as well as maps, sets, and access to files.

## Usage

Requires a JVM and [Mill](https://mill-build.org) (included as `./mill`), and for the compiler GMP and the Boehm garbage collector.

```
make                          # builds ulang.jar
./ulang.sh run file.u                     # checks a program and calls its procedure main
./ulang.sh test file.u                    # runs its tests one by one
./ulang.sh check file.u                   # only checks it
./mill ulang.test                         # runs all tests

make ulang.bin                            # builds the compiler from bootstrap/ulang.c
./ulang.bin compile file.u main file.c    # compiles a program to C, whose main calls the procedure main
cc -I self/runtime file.c self/runtime/ulang.c -lgmp -lgc
```

## Layout

- `ulang/src/` is the interpreter in Scala: parser, type checker, and evaluator.
- `self/` is the implementation in ulang: lexer, parser, checker, interpreter, and the compiler to C, with its runtime in `self/runtime/`.
- `bootstrap/ulang.c` is the C of the last compiler that compiled itself to the same C, from which `make ulang.bin` builds it without the JVM.
  `make selfcompile` rebuilds the compiler with itself, and `make bootstrap` with the interpreter.
- `tests/` are the tests, also of expected errors in `tests/errors/`.
- `vscode/` is an extension for VS Code and VSCodium, installed by `make install`.
