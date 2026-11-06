# TODO

Open action items, each with the problem and the desired solution.

## Testing

1. **The modules of the prelude cannot be compiled on their own**: `ulang.bin compile prelude/list.u test` fails with `sort List is already defined`, since every program has the prelude already, so `make testc` skips `prelude.u`, `option.u`, and `list.u`, whose tests run with the interpreter only, where `load_file` of `self/ulang.u` keeps only their tests. Solution: the same in `compile_file` of `self/compile.u`.

## Self-hosted toolchain

1. **`self/run.u` is slow on large programs**: interpreting the compiler on itself takes 121 seconds, three times as long as the Scala interpreter took, although it was faster on small programs. On the compiler compiling `tests/datatypes.u` (3.9 seconds), gprof attributes a quarter of the time in ulang code to `compare`, mostly from `map.get` on the environments keyed by `Name`, with about 70 million calls plus as many recursive ones; the collector takes another quarter of the whole run, 36 collections of 25 ms. Solution: number the names of declarations and variables when a program is loaded, so that keys compare as integers, since hash consing was deemed not worthwhile.
2. **No optimizations in the compiler.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, reading fields of single-constructor datatypes directly instead of calling their accessors, and loops for folds of comprehensions beyond `list.foldr` into lists, such as those of sets and maps and of several generators.

## Modules

1. **Imports are resolved against the directory of the parsed file**, not of the importing module, and loaded modules are known by name. Solution: resolve against the importing file, with modules known by their location, which makes nested imports and library modules consistent. Also imports from other directories, e.g. of `prove/` by `self/prove.u`, by relative paths or a search path.
2. **A private name of another module is unknown in messages**: since the signatures of modules have only their exported declarations, a use of a private declaration of an imported module says `unknown variable x`, not that x is private to its module. Solution: refine an unknown name on the path of the error in `check_unit` of `self/check.u` from the source of the visible modules, where a name of `implementation` that `export` does not sign is private, without private names in `Sig`.

## Compiler

1. **Runtime errors have no positions.** Solution: positions in the messages of the runtime.
2. **Deep recursion.** Functions like `concat` over long lists recurse deeply, as do `list_elements` and `str_values` in the interpreter, which convert the arguments of builtins such as `join`, so compiled programs need `ulimit -s unlimited` for big inputs. So does `foldr` of lists, where it is not one of the loops of comprehensions. Solution: loops in the runtime and library where possible, e.g. a native `foldr` of lists that applies its function from the last element to the first, and a large stack for the main thread of compiled programs, as the interpreter has.
3. **Integers are boxed**: every integer outside -16 to 255 is an object on the heap, so arithmetic allocates, e.g. 63 million calls of `u_mk_int` when `run.u` interprets the compiler. Solution: integers that fit in a word tagged in the pointer, with a check before every access to `type` and `num`.
4. **Programs need a C compiler**, and `cc -O2` takes most of the time of `make selfcompile`. Solution: a backend that writes assembly for x86-64, see `assembly.md`.

## Syntax

1. **Patterns are weaker than expressions**: no or-patterns `case TParam(_) | TMeta(_) =>` and no as-patterns, which forced `self/run.u` to rebuild values it had just taken apart. Solution: or-patterns and as-patterns. Before, the coverage of patterns of `self/check.u` (`Head`, `useful`, `missing`, `specialize`, about 300 lines), which the new patterns change, moves into a module `self/coverage.u` of its own, with its own tests, which needs only the constructors of types from `Sig` and the substitution of the checker.
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

1. **Contracts, lemmas, and proofs** are not implemented. Solution: parse specifications, lemmas, and proofs, and prove them with the simplifier of `prove/`.
2. **The simplifier of `prove/` reads its own syntax**, by `prove/read.u`, and does not see checked programs. Solution: a translation `self/prove.u` from the programs checked by `self/check.u`, with goals as `Bool` functions whose formals are the fixed variables, a default proof by simplification and induction on each formal of a datatype, where goals proven earlier are rules for later ones, and a command `ulang.bin prove <file>`, which needs the prover in `self/` or imports across directories.
