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
    case Leaf => List()
    case Node(l, k, r) => keys(l) ++ List(k; keys(r))
  end

// inserts the elements of xs into the search tree t, one at a time
procedure insert_all(in xs: List[Int], ref t: Tree[Int]) :=
  for x in xs do
    assign t := insert(x, t)
  end

procedure main() :=
  local t := Leaf

  call insert_all(in List(3, 1, 2, 1), ref t)
  call os_println(in show(keys(t)))

test sorting() :=
  local t := Node(Leaf, 2, Leaf)

  call   insert_all(in List(3, 1, 2), ref t)
  assert keys(t) == List(1, 2, 3)
```

## Language

- Functions are pure expressions, procedures are statements with `in`, `out`, and `ref` parameters.
- Functions and procedures are values: `lambda x => e`, whose formals may be patterns, as in `lambda n, (a, b) => n + a * b`, and `lambda in x, out y => statements end` of a type `(in A, out B)`, which sees the variables around it but assigns only its outs and refs.
- `if` always nests: an `if` after `else` is an `if` in its body, which has its own `end`.
- Datatypes with pattern matching, guards, and checks that matches are exhaustive and cases reachable.
- Named fields of constructors, read as `e.field` and updated as `e with field := a, other := b`, which are resolved by the type of `e`, and raise `MissingField` for a constructor without that field.
- Hindley-Milner type inference, where only declarations are polymorphic, with type parameters, and `let` does not generalize.
- Exceptions, values of datatypes without type parameters, raised by the statement `raise e`, and caught by `try`, which leave the variables as they were: outs and refs reach the caller only when a procedure returns, and handlers see the variables as they were before the `try`.
- Checked exceptions: `raises T, …` after an exported procedure and in procedure types, inferred for private procedures, where a `try` removes the types that its handlers cover, and a handler of a type that its body cannot raise is an error.
  Builtins raise `DivisionByZero`, `OutOfBounds`, and `NoCharacter`, which no `raises` counts, whereas a failed `assert` ends the program.
- Unbounded integers, characters, which are Unicode code points, and strings of them, which files hold as UTF-8.
- Programs read their arguments with `os_args`, files with `os_read` and `os_write`, and print strings with `os_print`, all of the module `os`, where the builtin `show` gives the text of any value, which a datatype may declare for itself by a function `show`.
- The forms `List(1, 2)`, `Set(a, b)`, `Map(k => v)`, and `Array(a, b)`, and their generators `List(for x in xs if x > 0 yield x * x)`, `Set(for x in c yield f(x))`, `Map(for k => v in m yield k => f(v))`, which range over lists, sets, arrays, or maps by their folds, and `==` compares their elements and entries; and the statement `for x in xs if p do … end`, which runs its body for the elements of any container.
- User-defined operators by `infix`, `prefix`, and `postfix` notations.
- Modules and packages: a file with `export` and `implementation` sections is a module, and the modules of a directory are a package, whose members see each other's exports without imports, also cyclically, while their private declarations stay private to their file. `import "p"` makes the exports of the package of the directory `p` available, by its path from the root of the project, the nearest directory above with `project.u`, but not transitively; packages do not import each other cyclically. A file without sections is a program, which sees the package of its directory, and every module sees the whole library without imports.
- Named tests `test name() := …`, whose parameters are reserved for later and must be empty, while programs start at a procedure without parameters, like `procedure main() := …`.
- Examples of functions and procedures, lines after the `:=` of the signature and the contract of a procedure, before the body, such as `example gcd(12, 18) == 6`, `example chr(-1) raises NoCharacter(-1)`, or `example divmod(in 7, 2, out 3, 1)`, which run as tests `gcd.example.1`, … in both backends.
- Lemmas `lemma l(in x: A, …) := requires a ensures b`, which `ulang.sh prove` proves and `ulang.sh test` first tests, as `l.lemma`, on all the values of the formals up to a small depth, with `Int` for type parameters, reporting a counterexample.
- Contracts of procedures, `requires a, …` and `ensures b` after the `:=`, and lemmas that claim them, `lemma sum_correct(in n: Int, out s: Int) := correct sum(in n, out s)`, with an `invariant e` per loop of the body as the proof, which `ulang.sh prove` proves for the runs that end, assuming the contracts of the procedures called.

The library in `ulang/prelude/` contains the prelude with natural numbers, options, and lists, as well as maps, sets, and access to files.

## Usage

Requires a C compiler, GMP, and the Boehm garbage collector. The interpreter and the compiler recurse as deeply as the programs they run, so `out/ulang.sh` runs `out/ulang.bin` with a large stack, by `ulimit -s unlimited`; compiled programs need the same limit.

```
make                                      # builds out/ulang.bin and out/ulang.sh from the sources, by the seed in bootstrap/,
                                          # and copies both to the root, so ./ulang.sh works as out/ulang.sh does
out/ulang.sh run file.u -- args           # checks a program and calls its procedure main
out/ulang.sh run file.u:go                # calls its procedure go instead
out/ulang.sh test file.u                  # runs its tests one by one
out/ulang.sh test file.u:name             # runs one of them
out/ulang.sh test dir                     # runs the files below dir, which must pass
out/ulang.sh test                         # runs the whole project, the root above with project.u, but
                                          # ulang/test/errors/, which project.u declares to run only compiled
out/ulang.sh compile --tests -o suite.c   # compiles the tests of the whole project, whose binary runs at the root
                                          # with the arguments of test
out/ulang.sh check file.u                 # only checks it
out/ulang.sh lint file.u                  # checks it and prints the lints, as warnings, with simpler expressions to write
out/ulang.sh format file.u                # rewrites it in the layout that check, lint, and test require
out/ulang.sh compile file.u -o file.c     # compiles a program to C, whose main calls the procedure main
out/ulang.sh help                         # lists the commands
cc -I ulang/runtime file.c ulang/runtime/ulang.c -lgmp -lgc
make test                                 # runs all tests
```

## Layout

- `ulang/` is the implementation in ulang, a package per phase, each importing those before it: `ulang/syntax/` with the errors that all phases share, `ulang/format/` the printer, `ulang/parse/` lexer and parser, `ulang/check/` the checker, `ulang/run/` the interpreter, `ulang/compile/` the compiler to C, with its runtime in `ulang/runtime/`, `ulang/prove/` a prover by contextual simplification over checked programs, `ulang/lint/` the lints, and the driver `ulang/ulang.u`.
- `bootstrap/ulang.c` is the seed, the C of the last compiler that compiled itself to the same C, from which `make` builds
  `bootstrap/ulang.bin`, which compiles the sources to `out/ulang.bin`. `make selfcompile` replaces the seed by the compiler
  compiled with itself, and `make bootstrap` by the compiler compiled with the interpreter. `bootstrap/ulang.sh` is the script
  that runs each binary with a large stack, copied next to it by `make`.
- `ulang/prelude/` is the library.
- The tests of each package are in its subfolder `test/`: `ulang/test/` of the language, `ulang/prove/test/` of the prover, and `ulang/prelude/test/` of the library. No file fails: the errors of the phases are tested in `ulang/test/errors/`, by cases, source texts in tests, whose exceptions `assert raises` expects.
- `vscode/` is an extension for VS Code and VSCodium, installed by `make install`.
- `TODO.md` lists the open work, and `DESIGN.md` the design notes.
