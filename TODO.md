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

1. **One namespace per module**: within a module, overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Solution: qualified names for modules (`check.infer`) or constructors (`Expr.Var`).
2. **Partial accesses are not visible**: `e.f` of a field that only some constructors have may raise `MissingField`, but nothing at the access shows it. Candidate: `e!f` for such fields and `e.f` for the others, as decided by the declaration and enforced by the checker. Reservations: one more rule to learn, and it reserves `!`.
3. **Constructors may have unnamed parameters**, so some fields have no accessors or updates, and their partiality is not known. Candidate: require names for all parameters of constructors, as in Scala's case classes, where patterns stay positional and tuples remain for anonymous products. Costs: more writing for small types, names that agree in their types across the constructors of `self/syntax.u`, and generated accessors and updaters, of which the compiler could drop the unused ones.
4. **`assign` is redundant** before `x := e`. Solution: decide whether to drop it.
5. **`[…]` is both type arguments and list literals.** Solution: decide whether to keep it.
6. **`else` followed by `if` always continues a chain of statements**, so a nested `if` cannot directly follow `else`. Solution: decide on a syntax that tells them apart.

## Fields and literals

1. **Accesses are applications of a variable `.f`** until the checker resolves them. Solution: a node `EField(e, f)` of the syntax.
2. **Negative literals take the position of the number**, not of the `-`. Solution: the position of the operator.

## Library

1. **The containers share only part of an interface**: lists, sets, maps, arrays, and strings convert into each other by names of their own, `from_list`, `put_all(ps, empty())`, `array`, `chars`, and `string`, with two names of one concept, `elements` and `list` of `set`, `entries` and `list` of `map`; sets have no `remove`, no `exists`, `forall`, `union`, `intersect`, or difference, and adding or taking away several elements has names of its own, `insert_all`, `put_all`, `remove_all`, and `diff` of lists; and sets and maps have no notation for adding to an existing one, as `[x; xs]` adds to a list. Solution, in one change, bootstrapped where the compiler uses a renamed function:
   1. Conversions named after the type they convert into, each taking the other type: `list(s: Set[A])` and `set(xs: List[A])` in `set.u`, `list(m: Map[K, V])` of the entries and `map(ps: List[K * V])`, where the last entry of a key counts, in `map.u`, `list(a: Array[A])` and `array(xs: List[A])` in `array.u`, and `list(s: Str): List[Char]` and `str(cs: List[Char])` in `str.u`, in place of `elements`, `from_list`, `entries`, `chars`, and `string`; a module defines one conversion of a name, since overloads within a module differ by arity only (Syntax 2), and conversions between two containers that are not lists go through `list`.
   2. `remove(x, s)` of sets, and `++` and `--` in place of `insert_all`, `put_all`, `remove_all`, and `diff`: `++` of two sets is their union, of two maps their entries, where those of the second count, as it already concatenates lists and strings, and `--` takes away, of two lists and of two sets the elements of the second, replacing `diff` of lists, and of a map the keys of a set, `m -- ks` with `ks: Set[K]`, for which `map.u` imports `set`; `++` stays right associative, and `--` binds more loosely, left associative as `-`, so that `a ++ b -- c` is `(a ++ b) -- c`, `a -- b ++ c` is `a -- (b ++ c)`, and `a -- b -- c` is `(a -- b) -- c`, while it binds more tightly than the comparisons, as `infix left 7 --`, the level between the comparisons (6) and `++` (8), which f2b0ff1 freed. Both operands of `++` and `--` have the same type, except the keys of `--` on maps, so that adding a list goes through a conversion, `s ++ set(xs)` and `m ++ map(ps)`, since overloads within a module differ by arity only (Syntax 2). Besides, `intersect(s, t)` of sets, and `exists(c, p)` and `forall(c, p)` of sets, maps, where `p` takes the key and the value, and arrays, by their folds.
   3. `{x; s}` for `insert(x, s)` and `{x, y; s}` for `insert(x, insert(y, s))`, and `{k => v; m}` for `put(k, v, m)` and `{k => v, l => w; m}` for `put(l, w, put(k, v, m))`, so that the last entry of a key counts, as the literals without a rest are translated now, in place of the error that a literal `{...}` has no rest after `;`, in expressions only, since sets and maps are abstract types without patterns.
   4. Arrays without `put`, which copies the array for one element, so that arrays are built whole, by `array`, literals, and comprehensions, and read by `at`, `get`, `slice`, and the folds, which discourages updating one element at a time.
   5. CLAUDE.md on the shared names, the conversions, and the notation, and `self/`, `tests/`, and `prove/` with the new names.

## Verification

1. **Contracts, lemmas, and proofs** are not implemented. Solution: parse specifications, lemmas, and proofs, and prove them with the simplifier of `prove/`.
2. **The simplifier of `prove/` reads its own syntax**, by `prove/read.u`, and does not see checked programs. Solution: a translation `self/prove.u` from the programs checked by `self/check.u`, with goals as `Bool` functions whose formals are the fixed variables, a default proof by simplification and induction on each formal of a datatype, where goals proven earlier are rules for later ones, and a command `ulang.bin prove <file>`, which needs the prover in `self/` or imports across directories.
