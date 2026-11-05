# TODO

Open action items, each with the problem and the desired solution.

## Testing

1. **The modules of the prelude cannot be compiled on their own**: `ulang.bin compile prelude/list.u test` fails with `sort List is already defined`, since every program has the prelude already, so `make testc` skips `prelude.u`, `option.u`, and `list.u`, whose tests run with the interpreter only, where `load_file` of `self/ulang.u` keeps only their tests. Solution: the same in `compile_file` of `self/compile.u`.

## Builtins

1. **Builtin functions are registered in three places**: their types in `builtin` of `self/check.u`, and their implementations in `self/run.u` and in the runtime with `builtin_functions` of `self/compile.u`, while the builtin procedures of `os` are declared by signatures without definitions in `prelude/os.u` and come with its import (`is_builtin_proc` in `self/check.u`). Solution: declare the functions the same way in library modules, e.g. `function str_get(s: Str, i: Int): Char` in a module of strings, so that only the implementations remain, and a builtin exists once its module is imported.

## Self-hosted toolchain

1. **No positions in `self/syntax.u`**, so `check.u` and `compile.u` cannot say where an error is, although the parser knows offsets. Solution: a position per command, and later per statement and expression.
2. **The parser has no tests of single constructs**, only the round trip of `make testformat` over all files. Solution: tests of `self/parse.u` that parse single constructs and compare them with their expected syntax, or with the source that `show_program` prints, e.g. chains of operators of each associativity, negative literals, displays with rests, literals and comprehensions of sets and maps, and the errors that only the parser reports.
3. **`self/run.u` is slow on large programs**: interpreting the compiler on itself takes 121 seconds, three times as long as the Scala interpreter took, although it was faster on small programs. On the compiler compiling `tests/datatypes.u` (3.9 seconds), gprof attributes a quarter of the time in ulang code to `compare`, mostly from `map.get` on the environments keyed by `Name`, with about 70 million calls plus as many recursive ones; the collector takes another quarter of the whole run, 36 collections of 25 ms. Solution: number the names of declarations and variables when a program is loaded, so that keys compare as integers, since hash consing was deemed not worthwhile.
4. **Ambiguities of overloads by arity list a name twice**: `local v := f` of two functions `f` of one module says `f is ambiguous: m.f, m.f`. Solution: the candidates with their types in the message.
5. **Messages of `self/check.u` are less specific than they were in Scala**, e.g. `duplicate x` for `duplicate assignment to x`, `expected 1 values, got 2` without the constructor or procedure, and parse errors such as `pattern expected but then found` for `keyword then is not a name`, see the first lines of `tests/errors/`. Solution: say what is duplicated or miscounted, and name keywords.
6. **No optimizations in the compiler.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, reading fields of single-constructor datatypes directly instead of calling their accessors, and loops for folds of comprehensions beyond `list.foldr` into lists, such as those of sets and maps and of several generators.

## Modules

1. **Type errors in imported modules have no file**: they show a position without the module. Solution: add the module to the messages, as parse errors do.
2. **Imports are resolved against the directory of the parsed file**, not of the importing module, and loaded modules are known by name. Solution: resolve against the importing file, with modules known by their location, which makes nested imports and library modules consistent. Also imports from other directories, e.g. of `simp/` by `self/prove.u`, by relative paths or a search path.

## Compiler

1. **Runtime errors have no positions.** Solution: positions in the messages of the runtime.
2. **Deep recursion.** Functions like `concat` and `str_join` over long lists recurse deeply, so compiled programs need `ulimit -s unlimited` for big inputs. So does `foldr` of lists, where it is not one of the loops of comprehensions. Solution: loops in the runtime and library where possible, e.g. a native `foldr` of lists that applies its function from the last element to the first, and a large stack for the main thread of compiled programs, as the interpreter has.
3. **Integers are boxed**: every integer outside -16 to 255 is an object on the heap, so arithmetic allocates, e.g. 63 million calls of `u_mk_int` when `run.u` interprets the compiler. Solution: integers that fit in a word tagged in the pointer, with a check before every access to `type` and `num`.
4. **Programs need a C compiler**, and `cc -O2` takes most of the time of `make selfcompile`. Solution: a backend that writes assembly for x86-64, see `assembly.md`.

## Syntax

1. **Patterns are weaker than expressions**: no or-patterns `case TParam(_) | TMeta(_) =>` and no as-patterns, which forced `self/run.u` to rebuild values it had just taken apart. Solution: or-patterns and as-patterns.
2. **One namespace per module**: within a module, overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Solution: qualified names for modules (`check.infer`) or constructors (`Expr.Var`).
3. **Partial accesses are not visible**: `e.f` of a field that only some constructors have may raise `MissingField`, but nothing at the access shows it. Candidate: `e!f` for such fields and `e.f` for the others, as decided by the declaration and enforced by the checker. Reservations: one more rule to learn, and it reserves `!`.
4. **Constructors may have unnamed parameters**, so some fields have no accessors or updates, and their partiality is not known. Candidate: require names for all parameters of constructors, as in Scala's case classes, where patterns stay positional and tuples remain for anonymous products. Costs: more writing for small types, names that agree in their types across the constructors of `self/syntax.u`, and generated accessors and updaters, of which the compiler could drop the unused ones.
5. **`assign` is redundant** before `x := e`. Solution: decide whether to drop it.
6. **`[…]` is both type arguments and list literals.** Solution: decide whether to keep it.
7. **`else` followed by `if` always continues a chain of statements**, so a nested `if` cannot directly follow `else`. Solution: decide on a syntax that tells them apart.

## Fields and literals

1. **Accesses are applications of a variable `.f`** until the checker resolves them. Solution: a node `EField(e, f)` of the syntax.
2. **Negative literals take the position of the number**, not of the `-`. Solution: the position of the operator.

## Library

1. **Lists, sets, and maps share only part of an interface**: `size`, `contains`, `get`, `remove`, `foldr`, and `foldl` are common, and `set` and `map` have `empty` and `list`, but `set` has no `remove`, lists have no `empty`, `insert`, or `from_list`, and maps have `put_all` instead of `from_list`. Solution: the same names for all three, where the elements of maps are their entries, so that code reads alike for each container.

## Verification

1. **Contracts, lemmas, and proofs** are designed in `DESIGN.md`, but not implemented. Solution: implement them, starting with parsing specifications, and decide the open points listed there.
2. **No simplifier**: the proofs of `DESIGN.md` rest on contextual simplification of nested sequents with CC(X), designed there under Simplification. Solution: implement it in `simp/`, in the order `term.u`, `seq.u`, `cc.u` with `constr.u`, `arith.u`, and `simp.u`, with its tests run by `make test` and `make testc`, and then `self/prove.u` with goals as `Bool` functions and a command `simp`.
