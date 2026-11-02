# TODO

Open action items, each with the problem and the desired solution.

## Command line

1. **The drivers differ, and their options are flags**: `./ulang.sh` runs files and takes `--tests` and `--only`, while `./ulang.bin` only compiles; `eval` prints values, runs print their variables, and programs print only with `os_print`, which prints any value as `Printer.show` does. Solution, step by step, each committed with all checks passing:
   1. Drop `eval` from the language, and runs no longer print their variables.
   2. Commands as in git, the same for `./ulang.sh` and `./ulang.bin`: `ulang run <file> [<name>] [-- args]`, `ulang test <file> [<name> ...] [-- args]`, `ulang check <file>`, and `ulang compile <file> <entry> <target.c>`, with exit status 1 on failure, first in `Main`, where `compile` runs `self/ulang.u` in the interpreter, with the Makefile, the VS Code extension, and the docs.
   3. The compiler with an entry: `main()` runs the run `<entry>`, or all tests for the entry `test`, which `make selftest` uses.
   4. `self/ulang.u` as the driver with the same commands, on `run.u` with the arguments of the program, selected runs and tests, and errors with positions.
   5. Both drivers with the same output on `tests/`, and then the time of `run.u` on the compiler.

## Builtins

1. **Builtins are registered in five places**: their types in `Check.scala` and `self/check.u`, and their implementations in `Run.scala`, `self/run.u`, and the runtime with `builtin_functions` of `self/compile.u`. Solution: declare their signatures in library modules, e.g. `builtin function str_get(s: Str, i: Int): Char`, so that only the implementations remain, and a builtin exists once its module is imported.
2. **Builtins are enabled by datatype names**: `read` and `library` by any datatype named `Program`, so a program with its own `data Program := Foo(Int)` type-checks `call read(…)` and fails at run time, and the `os_` procedures by `OsError`. They are also special-cased by name in `Check.procedure` and `Run.invoke`. Solution: enable them by the import of their module, e.g. with `syntax.u` in the library, as part of item 1.

## Self-hosted toolchain

1. **No positions in `self/syntax.u`**, so `check.u` and `compile.u` cannot say where an error is, although the parser knows offsets. Solution: a position per command, and later per statement and expression.
2. **Errors of the driver**: `LexError`, `ParseError`, `TypeError`, `RuntimeError`, `CompileError`, and `OsError` leave `ulang.u` as uncaught exceptions, so a parse error shows a raw offset like `ParseError("…", 1234)` instead of a line and column, although the parser knows its offsets, and in an imported module only its name is added. Solution: catch them in `ulang.u` and print a message with the file, line, and column, which `position` of `lex.u` computes from the offset and the text of the module; this suffices for lexing and parsing, while type and compile errors also need item 1.
3. **The driver prints `args`**, since a run shows its local variables. Solution: a way to run a program without showing its locals, e.g. a run that calls a procedure `main`, whose locals are not shown.
4. **Paths read at run time** are relative to the program in the interpreter, but to the working directory in compiled programs, so the same program reads different files. Solution: resolve them relative to the working directory in both, and run each test file from its directory, as `make selftest` does.
5. **The file lists in the tests of `parse.u`** are written out, so new test files must be added by hand. Solution: a builtin `os_list` that lists a directory.
6. **`read`, `library`, and `Quote`** remain only as the oracle of the tests of `parse.u`, and as stubs in the runtime. Solution: compare `parse.u` with the Scala parser without them, e.g. by printing both syntaxes to text, and then remove them.
7. **Prefixes against clashes** in `lex.u` and `parse.u`, which predate modules. Solution: drop them where no module imports both modules of a clash.
8. **No optimizations in the compiler.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, reading fields of single-constructor datatypes directly instead of calling their accessors, and avoiding the intermediate lists of comprehensions.

## Modules

1. **Type errors in imported modules have no file**: they show a position without the module. Solution: add the module to the messages, as parse errors do.
2. **Imports are resolved against the directory of the parsed file**, not of the importing module, in both parsers, and loaded modules are known by name. Solution: resolve against the importing file, with modules known by their location, which makes nested imports and library modules consistent.
3. **All declarations are exported**, so helpers of different modules only coexist if no module imports both. Solution: explicit exports.

## The read procedure

1. **Only missing files are `OsError`s.** Other I/O failures, e.g. permissions or a path that `os.Path` rejects, throw Java exceptions that `Main` and `FileTest` do not catch. Solution: raise `OsError` for every I/O failure.
2. **Parse errors in the read file** are reported against the reading program, without the read file or the call site, and cannot be caught by `try`. Solution: raise them as an exception with the file and position.
3. **Wrong arguments to `read`** fall through to the message "unknown procedure read". Solution: a message about the arguments.

## Compiler

1. **Variables after a raise.** `try` uses `setjmp`, after which C leaves local variables that changed since then undefined unless they are `volatile`; outs and refs work in practice since their address is taken, but this is not guaranteed. Solution: declare the locals that a `try` assigns as `volatile`.
2. **Compiled programs are compared with the interpreter by hand**, while the test of `compile.u` only writes their translations. Solution: a make target or test that compiles `self/out/*.c` and compares their output with the interpreter.
3. **Runtime errors have no positions**, and an unassigned variable is a `NULL` pointer instead of an error. Solution: positions in the messages of the runtime, and a check of unassigned variables.
4. **Deep recursion.** Functions like `concat` and `str_join` over long lists recurse deeply, so compiled programs need `ulimit -s unlimited` for big inputs. Solution: loops in the runtime and library where possible, and a large stack for the main thread of compiled programs, as the interpreter has.

## Syntax

1. **Repeated loops in `self/parse.u`**: the operator loop three times, `cases` twice, and the generators of comprehensions in `parse_brackets` and `set_comprehension`. Solution: write each once, taking the parser of an item as a procedure value, as `sections` does.
2. **Patterns are weaker than expressions**: no or-patterns `case TParam(_) | TMeta(_) =>` and no as-patterns, which forced `self/run.u` to rebuild values it had just taken apart. Solution: or-patterns and as-patterns.
3. **One global namespace**: overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Solution: qualified names for modules (`check.infer`) or constructors (`Expr.Var`).
4. **No overloading by types**, so lists, sets, and maps need different names (`length`, `count`, `size`, `contains`, `member`, `has`, `lookup`, `get`), `{}` cannot be the empty set or map, and `==` and `compare` of sets and maps compare the shapes of their trees, not their elements. Solution: overloading by types, e.g. by interfaces that lists, sets, and maps implement.
5. **Partial accesses are not visible**: `e.f` of a field that only some constructors have may raise `MissingField`, but nothing at the access shows it. Candidate: `e!f` for such fields and `e.f` for the others, as decided by the declaration and enforced by the checker. Reservations: one more rule to learn, and it reserves `!`.
6. **Constructors may have unnamed parameters**, so some fields have no accessors or updates, and their partiality is not known. Candidate: require names for all parameters of constructors, as in Scala's case classes, where patterns stay positional and tuples remain for anonymous products. Costs: more writing for small types, names that agree in their types across the constructors of `self/syntax.u`, and generated accessors and updaters, of which the compiler could drop the unused ones.
7. **`assign` is redundant** before `x := e`. Solution: decide whether to drop it.
8. **`[…]` is both type arguments and list literals.** Solution: decide whether to keep it.
9. **`else` followed by `if` always continues a chain of statements**, so a nested `if` cannot directly follow `else`. Solution: decide on a syntax that tells them apart.

## Fields and literals

1. **Accessors need known types**: `e.f` needs the type of `e` when the access is inferred, so `lambda p => p.x` fails with an unknown type. Solution: resolve accesses at the end of the command, with a fresh type for the result in the meantime.
2. **Accesses are applications of a variable `.f`** until the checker resolves them. Solution: a node `EField(e, f)` of the syntax, together with item 1.
3. **Literals resolve to fixed modules**: `{a, b}` and `{k => v}` call `set.from_list` and `map.put_all` by qualified names, so a local `set.u` takes their place, and a module may use them without importing `set` or `map`. Solution: resolve them only through the module's own imports, or by overloading (Syntax 4).
4. **Misleading errors after `for`**: in `Parser.scala`, a generator of a set or map comprehension that does not parse backtracks into the literal, which reports `} expected` at `for`, unlike `self/parse.u`. Solution: commit to the comprehension after `for`.
5. **Matches in map comprehensions repeat** for the result, the condition, and the map of each later generator. Solution: patterns in generators, `for (k, v) in entries(m)`, which bind them once and remove the hidden variables.
6. **Negative literals take the position of the number**, not of the `-`. Solution: the position of the operator.
7. **`nil_expr()` in `self/parse.u`** is still written out where `parse_brackets` builds `Nil`. Solution: use it there.
8. **`literal` and `comprehension` in `Parser.scala`** use a `match` of guards only and iterator chains. Solution: recursion and nested matches.

## Verification

1. **Contracts, lemmas, and proofs** are designed in `DESIGN.md`, but not implemented. Solution: implement them, starting with parsing specifications in both parsers, and decide the open points listed there.
