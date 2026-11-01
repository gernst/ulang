# TODO

## Self-hosting

The parts in `self/` form a compiler from ulang to C written in ulang, which compiles itself.

- `lex.u` and `parse.u` parse as `Parser.scala` does, which their tests check against `read` and `library` on every file of `tests/`, `examples/`, `self/`, and the library.
- `check.u` with `ctx.u` checks as `Check.scala` does, and `run.u` interprets as `Run.scala` does.
- `c.u` and `compile.u` translate checked programs to C, which runs with `self/runtime` (GMP, the Boehm collector, and `setjmp` for exceptions).
- `ulang.u` is the driver: `./ulang.sh self/ulang.u -- source.u target.c` from the Scala interpreter, or `ulang source.u target.c` once compiled, with paths relative to `self/` for the interpreter.
- The compiled compiler produces the same C as the interpreted one on the tests and examples, in a fraction of a second instead of minutes.

### Open points

1. **The bootstrap reaches a fixed point**: the interpreter compiles `ulang.u` to `ulang1.c` in about 8 minutes, whose program compiles `ulang.u` to `ulang2.c` in about 35 seconds, and both are identical. This has no make target or test yet, and 35 seconds for 400 KB of C suggests quadratic list operations, e.g. `++` on growing lists or association lists as maps.
2. **Builtins are registered in five places**: their types in `Check.scala` and `self/check.u`, and their implementations in `Run.scala`, `self/run.u`, and the runtime with `builtin_functions` of `self/compile.u`. Signatures declared in library modules, e.g. `builtin function str_get(s: Str, i: Int): Char`, would leave only the implementations, and would make a builtin exist once its module is imported.
3. **Builtins are enabled by datatype names**: `read` and `library` by any datatype `Program`, the `os_` procedures by `OsError`. Enabling them by the import of their module would be sturdier, e.g. with `syntax.u` in the library.
4. **One global namespace across modules.** Hooking up the parser hit clashes of `literal`, `resolve`, `formals`, and `expr_cases`, now avoided by prefixes. Helpers that an import does not export would avoid this.
5. **No positions in `self/syntax.u`.** The parser knows offsets, but `check.u` and `compile.u` cannot say where an error is. A position per command would be a start.
6. **Errors of the driver.** `LexError`, `ParseError`, `TypeError`, `RuntimeError`, `CompileError`, and `OsError` leave `ulang.u` as uncaught exceptions, instead of a message with a position.
7. **The driver prints `args`**, since a run shows its local variables.
8. **Imports are resolved against the directory of the parsed file**, not of the importing module, in both parsers, and loaded modules are known by name. Resolving against the importing file, with modules known by their location, would make nested imports and library modules consistent, and make the base name of the parsed file in the loaded modules unnecessary.
9. **Paths read at run time** are relative to the program in the interpreter, but to the working directory in compiled programs.
10. **The file lists in the tests of `parse.u`** are written out, since there is no `os_list`.
11. **`read`, `library`, and `Quote`** only remain as the oracle of the tests of `parse.u`, and as stubs in the runtime, since imported procedures that call them are compiled.
12. **Parsing is slow in the interpreter**: compiling the compiler with it takes about ten minutes.

## The read procedure

1. **Any datatype named `Program` enables `read`**, not only the one of `self/syntax.u`. A program with its own `data Program := Foo(Int)` type-checks `call read(in path, out p)` and then fails at runtime, since `p` is a `Program(…)` from `Quote`.
2. **Only missing files are `OsError`s.** Other I/O failures, e.g. permissions or a path that `os.Path` rejects, throw Java exceptions that `Main` and `FileTest` do not catch.
3. **Parse errors in the read file** are reported against the reading program, without the read file or the call site, and cannot be caught by `try`.
4. **Builtin procedures are special-cased by name** in `Check.procedure` and `Run.invoke`, see the builtins above.
5. **Wrong arguments to `read`** fall through to the message "unknown procedure read".

## Compiler

1. **Variables after a raise.** `try` uses `setjmp`, after which C leaves local variables that changed since then undefined unless they are `volatile`. Variables whose address is taken, as for outs and refs, work in practice, but this is not guaranteed.
2. **Compiled programs are compared with the interpreter by hand**, while the test only writes their translations. The comparison could become part of the tests.
3. **Runtime errors have no positions**, and an unassigned variable is a `NULL` pointer instead of an error.
4. **Deep recursion.** Functions like `concat` and `str_join` over long lists recurse deeply, and compiled programs need a large stack (`ulimit -s unlimited`) for big inputs, as the interpreter runs programs on a thread with a large stack.

## Syntax

Points of a critique of the syntax, ordered by how much they cost when writing `self/check.u`, `self/run.u`, and `self/parse.u`.

1. **Records.** Datatypes have only positional fields, so `Ctx` in `self/ctx.u` needs an accessor and an update function per component, each matching `Ctx(a, f, p, d, m, k)`. Named fields would replace them: `c.funs`, `c with funs := …`, `Ctx { funs := f }`.
2. **Locals.** Every result of a procedure call needs a local, declared with a type at the beginning of a block. Inferring the type from an initial value would help: `local x := e`.
3. **No parsers as values.** Procedures are not values, and functions cannot fail, so `self/parse.u` repeats each loop of `repsep` and `rep1sep` per kind of item, and has the operator loop three times and `cases` twice, which makes it more than twice as long as `Parser.scala`.
4. **Patterns are weaker than expressions.** There are no or-patterns `case TParam(_) | TMeta(_) =>`, or as-patterns, which forced `self/run.u` to rebuild values it had just taken apart.
5. **One global namespace.** Functions, procedures, and constructors share one namespace, and overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Qualified names for modules (`check.infer`) or constructors (`Expr.Var`) would help.
6. **Smaller points.**
   - `assign` is redundant before `x := e`.
   - `[…]` is both type arguments and list literals.
   - `else` followed by `if` always continues a chain of statements, so a nested `if` cannot directly follow `else`.

Records, then `local` with an initial value, would shorten `self/check.u` the most.

## Design notes

What a language would ideally offer for writing code with an AI assistant, which types cheaply but makes mistakes it cannot see: implicit conversions, shadowed names, hidden effects, forgotten cases.

1. **Everything that matters is visible where it happens**: effects and data flow in signatures, no implicit conversions or overloading by type, and failure in the type, with short propagation like Rust's `?`.
2. **A strict, fast, and precise checker**: inference within functions, types at module boundaries, exhaustive matches, and errors with positions and expected types, since the checker is the feedback loop.
3. **Small and uniform**: few concepts, one way to write each, no context-dependent syntax, and a canonical formatter.
4. **Modules with explicit exports, and records with named fields**, since names are how code is navigated, and clashes and positions are where mistakes happen.
5. **Abstraction over effectful code**, such that patterns like `repsep` are written once.
6. **Specifications in the language**: pre- and postconditions and properties, checked by tests or a prover, since the common mistakes are subtle logic errors rather than type errors.

### Compared to ulang

- **Already there**: explicit data flow with `in`, `out`, and `ref`, pure expressions with effects only in statements, exhaustive matches and inference, a small language without implicit behavior, exceptions as values of a type, and named tests with `assert` showing both sides.
- **Missing**: records, modules with exports, procedures as values or functions that can fail, `local x := e`, positions in errors, and contracts. These are extensions, not redesigns, see the points above.
- **Questionable**: user-defined notations make the syntax depend on declarations, e.g. whether `and` is an operator or a name, for tools and for writing code alike.
