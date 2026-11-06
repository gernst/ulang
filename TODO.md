# TODO

Open action items, each with the problem and the desired solution.

## Testing

1. **The modules of the prelude cannot be compiled on their own**: `ulang.bin compile prelude/list.u test` fails with `sort List is already defined`, since every program has the prelude already, so `make testc` skips `prelude.u`, `option.u`, and `list.u`, whose tests run with the interpreter only, where `load_file` of `self/ulang.u` keeps only their tests. Solution: the same in `compile_file` of `self/compile.u`.

## Self-hosted toolchain

1. **No optimizations in the compiler.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, reading fields of single-constructor datatypes directly instead of calling their accessors, and loops for folds of comprehensions beyond `list.foldr` into lists, such as those of sets and maps and of several generators.

## Modules

1. **Imports are resolved against the directory of the parsed file**, not of the importing module, and loaded modules are known by name. Solution: resolve against the importing file, with modules known by their location, which makes nested imports and library modules consistent. Also imports from other directories, e.g. of `prove/` by `self/prove.u`, by relative paths or a search path.
2. **A private name of another module is unknown in messages**: since the signatures of modules have only their exported declarations, a use of a private declaration of an imported module says `unknown variable x`, not that x is private to its module. Solution: refine an unknown name on the path of the error in `check_unit` of `self/check.u` from the source of the visible modules, where a name of `implementation` that `export` does not sign is private, without private names in `Sig`.

## Compiler

1. **Runtime errors have no positions.** Solution: positions in the messages of the runtime.
2. **Deep recursion.** Functions like `concat` over long lists recurse deeply, as do `list_elements` and `str_values` in the interpreter, which convert the arguments of builtins such as `join`, so compiled programs need `ulimit -s unlimited` for big inputs. So does `foldr` of lists, where it is not one of the loops of comprehensions. Solution: loops in the runtime and library where possible, e.g. a native `foldr` of lists that applies its function from the last element to the first, and a large stack for the main thread of compiled programs, as the interpreter has.
3. **Programs need a C compiler**, and `cc -O2` takes most of the time of `make selfcompile`. Solution: a backend that writes assembly for x86-64, see `assembly.md`.

## Syntax

1. **One namespace per module**: within a module, overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Solution: qualified names for modules (`check.infer`) or constructors (`Expr.Var`).
2. **Constructors may have unnamed parameters**, so some fields have no accessors or updates, and their partiality is not known. Candidate: require names for all parameters of constructors, as in Scala's case classes, where patterns stay positional and tuples remain for anonymous products. Costs: more writing for small types, names that agree in their types across the constructors of `self/syntax.u`, and generated accessors and updaters, of which the compiler could drop the unused ones.
3. **`else` followed by `if` always continues a chain of statements**, so a nested `if` cannot directly follow `else`. Solution: decide on a syntax that tells them apart.

## Fields and literals

1. **Accesses are applications of a variable `.f`** until the checker resolves them. Solution: a node `EField(e, f)` of the syntax.


## Verification

1. **Contracts, lemmas, and proofs** are not implemented. Solution: parse specifications, lemmas, and proofs, and prove them with the simplifier of `prove/`.
2. **The simplifier of `prove/` reads its own syntax**, by `prove/read.u`, and does not see checked programs. Solution: a translation `self/prove.u` from the programs checked by `self/check.u`, with goals as `Bool` functions whose formals are the fixed variables, a default proof by simplification and induction on each formal of a datatype, where goals proven earlier are rules for later ones, and a command `ulang.bin prove <file>`, which needs the prover in `self/` or imports across directories.

## Rejected and decided

- **A faster `self/run.u`** by numbering the names of declarations and variables when a program is loaded, so that the environments compare integers, and **integers tagged in the pointer** in the runtime, to save the boxes of most integers: both rejected.
- **Partial field accesses** marked at the access, as `e!f`: dropped.
- **`assign`** stays before `x := e`, and **`[…]`** stays for both type arguments and list literals.
