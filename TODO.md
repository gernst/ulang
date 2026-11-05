# TODO

Open action items, each with the problem and the desired solution.

## Look at
- module imports and exports

## Testing

1. **Compiled and interpreted programs are compared by hand**: `make test` runs the interpreter only, and nothing checks that the compiled tests print what `./ulang.bin test` prints, where they differ for `tests/exceptions.u` (Compiler 1). Solution: a make target that compiles each file of `tests/` with the entry `test` and compares its output with `./ulang.bin test`.

## Builtins

1. **Builtins are registered in three places**: their types in `self/check.u`, and their implementations in `self/run.u` and in the runtime with `builtin_functions` of `self/compile.u`. Solution: declare their signatures in library modules, e.g. `builtin function str_get(s: Str, i: Int): Char`, so that only the implementations remain, and a builtin exists once its module is imported.
2. **Builtins are enabled by datatype names**: the `os_` procedures by any datatype `OsError`, and they are special-cased by name in `call_declared` of `self/check.u` and in `self/run.u`. Solution: enable them by the import of their module, as part of item 1.

## Self-hosted toolchain

1. **No positions in `self/syntax.u`**, so `check.u` and `compile.u` cannot say where an error is, although the parser knows offsets. Solution: a position per command, and later per statement and expression.
2. **The file lists in the tests of `parse.u`** are written out, so new test files must be added by hand. Solution: `os_walk` of the module `os`.
3. **The tests of `parse.u` only check that files parse**, since its oracle, the Scala parser through `read`, is gone. Solution: a round trip, parsing each file, printing its syntax with `self/print.u`, and parsing that again to the same syntax.
4. **Prefixes against clashes** in `lex.u` and `parse.u`, which predate modules. Solution: drop them where no module imports both modules of a clash.
5. **`self/run.u` is slow on large programs**: interpreting the compiler on itself takes 121 seconds, three times as long as the Scala interpreter took, although it was faster on small programs. On the compiler compiling `tests/datatypes.u` (3.9 seconds), gprof attributes a quarter of the time in ulang code to `compare`, mostly from `map.get` on the environments keyed by `Name`, with about 70 million calls plus as many recursive ones; the collector takes another quarter of the whole run, 36 collections of 25 ms. Solution: number the names of declarations and variables when a program is loaded, so that keys compare as integers, since hash consing was deemed not worthwhile.
6. **Ambiguities of overloads by arity list a name twice**: `local v := f` of two functions `f` of one module says `f is ambiguous: m.f, m.f`. Solution: the candidates with their types in the message.
7. **Messages of `self/check.u` are less specific than they were in Scala**, e.g. `duplicate x` for `duplicate assignment to x`, `expected 1 values, got 2` without the constructor or procedure, and parse errors such as `pattern expected but then found` for `keyword then is not a name`, see the first lines of `tests/errors/`. Solution: say what is duplicated or miscounted, and name keywords.
8. **No optimizations in the compiler.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, reading fields of single-constructor datatypes directly instead of calling their accessors, and avoiding the intermediate lists of comprehensions.

## Modules

1. **Type errors in imported modules have no file**: they show a position without the module. Solution: add the module to the messages, as parse errors do.
2. **Imports are resolved against the directory of the parsed file**, not of the importing module, and loaded modules are known by name. Solution: resolve against the importing file, with modules known by their location, which makes nested imports and library modules consistent. Also imports from other directories, e.g. of `simp/` by `self/prove.u`, by relative paths or a search path.
3. **All declarations are exported**, so helpers of different modules only coexist if no module imports both. Solution: explicit exports.

## Compiler

1. **Variables after a raise.** `try` uses `setjmp`, after which C leaves local variables that changed since then undefined unless they are `volatile`; outs and refs work in practice since their address is taken, but this is not guaranteed. Solution: declare the locals that a `try` assigns as `volatile`.
2. **Runtime errors have no positions**, and an unassigned variable is a `NULL` pointer instead of an error. Solution: positions in the messages of the runtime, and a check of unassigned variables.
3. **Deep recursion.** Functions like `concat` and `str_join` over long lists recurse deeply, so compiled programs need `ulimit -s unlimited` for big inputs. Solution: loops in the runtime and library where possible, and a large stack for the main thread of compiled programs, as the interpreter has.
4. **Integers are boxed**: every integer outside -16 to 255 is an object on the heap, so arithmetic allocates, e.g. 63 million calls of `u_mk_int` when `run.u` interprets the compiler. Solution: integers that fit in a word tagged in the pointer, with a check before every access to `type` and `num`.

## Syntax

1. **Repeated loops in `self/parse.u`**: the operator loop three times, `cases` twice, and the generators of comprehensions in `parse_brackets` and `set_comprehension`. Solution: write each once, taking the parser of an item as a procedure value, as `sections` does.
2. **Patterns are weaker than expressions**: no or-patterns `case TParam(_) | TMeta(_) =>` and no as-patterns, which forced `self/run.u` to rebuild values it had just taken apart. Solution: or-patterns and as-patterns.
3. **One namespace per module**: within a module, overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Solution: qualified names for modules (`check.infer`) or constructors (`Expr.Var`).
4. **`{}` cannot denote the empty set or map.** Solution: `{}` as `empty()`, resolved by the expected type among the overloads of `set` and `map`.
5. **Partial accesses are not visible**: `e.f` of a field that only some constructors have may raise `MissingField`, but nothing at the access shows it. Candidate: `e!f` for such fields and `e.f` for the others, as decided by the declaration and enforced by the checker. Reservations: one more rule to learn, and it reserves `!`.
6. **Constructors may have unnamed parameters**, so some fields have no accessors or updates, and their partiality is not known. Candidate: require names for all parameters of constructors, as in Scala's case classes, where patterns stay positional and tuples remain for anonymous products. Costs: more writing for small types, names that agree in their types across the constructors of `self/syntax.u`, and generated accessors and updaters, of which the compiler could drop the unused ones.
7. **`assign` is redundant** before `x := e`. Solution: decide whether to drop it.
8. **`[…]` is both type arguments and list literals.** Solution: decide whether to keep it.
9. **`else` followed by `if` always continues a chain of statements**, so a nested `if` cannot directly follow `else`. Solution: decide on a syntax that tells them apart.

## Fields and literals

1. **Accesses are applications of a variable `.f`** until the checker resolves them. Solution: a node `EField(e, f)` of the syntax.
2. **Literals resolve to fixed modules**: `{a, b}` and `{k => v}` call `set.from_list` and `map.put_all` by qualified names, so a local `set.u` takes their place, and a module may use them without importing `set` or `map`. Solution: resolve them only through the module's own imports, or by overloading (Syntax 4).
3. **Matches in map comprehensions repeat** for the result, the condition, and the map of each later generator. Solution: patterns in generators, `for (k, v) in entries(m)`, which bind them once and remove the hidden variables.
4. **Negative literals take the position of the number**, not of the `-`. Solution: the position of the operator.
5. **`nil_expr()` in `self/parse.u`** is still written out where `parse_brackets` builds `Nil`. Solution: use it there.

## Verification

1. **Contracts, lemmas, and proofs** are designed in `DESIGN.md`, but not implemented. Solution: implement them, starting with parsing specifications, and decide the open points listed there.
2. **No simplifier**: the proofs of `DESIGN.md` rest on contextual simplification of nested sequents with CC(X), designed there under Simplification. Solution: implement it in `simp/`, in the order `term.u`, `seq.u`, `cc.u` with `constr.u`, `arith.u`, and `simp.u`, with its tests run by `./mill ulang.test` and `make selftest`, and then `self/prove.u` with goals as `Bool` functions and a command `simp`.
