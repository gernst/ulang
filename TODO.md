# TODO

Open action items, each with the problem and the desired solution.

## Testing

1. **The modules of the prelude cannot be compiled on their own**: `ulang.bin compile prelude/list.u test` fails with `sort List is already defined`, since every program has the prelude already, so `make testc` skips `prelude.u`, `option.u`, and `list.u`, whose tests run with the interpreter only, where `load_file` of `self/ulang.u` keeps only their tests. Solution: the same in `compile_file` of `self/compile.u`.
2. **Properties are only tested on the examples written by hand**, and the formals of tests are reserved but rejected. Solution: tests with formals, `test append_size(xs: List[Int], ys: List[Int]) := assert …`, which the runner calls on generated values of the types of the formals, deterministically, and which report the values of a failure. They give the evidence "tested" of Verification 3.

## Self-hosted toolchain

1. **No optimizations in the compiler.** Solution: passes between checking and translating to C, e.g. inlining small functions, folding constants, reading fields of single-constructor datatypes directly instead of calling their accessors, and loops for folds of comprehensions beyond `list.foldr` into lists, such as those of sets and maps and of several generators.

## Modules

1. **Imports are resolved against the directory of the parsed file**, not of the importing module, and loaded modules are known by name. Solution: resolve against the importing file, with modules known by their location, which makes nested imports and library modules consistent. Also imports from other directories, e.g. of `prove/` by `self/prove.u`, by relative paths or a search path.

## Overloading

1. **The heuristics of resolution are hard to predict**: when several candidates fit, the checker takes the most specific, else the most general, else the one of the module itself, and else the one after which the other open uses of the command still have candidates, so that the meaning of a use may depend on other uses and on the imports. Solution: a use resolves only by the types of its arguments and its expected type, deferred until they are known, and must end with exactly one candidate, else it is an error that asks for a qualified name or an annotation. Declarations of `compare`, `==`, and `show` for a datatype are then not candidates of uses of the builtins, which reach them at run time through the descriptor of the type anyway.
2. **The module `*`**: literals and comprehensions of sets and maps translate to `insert`, `put`, `empty`, `foldr`, and `foldl` of the module `*`, which overloading resolves through the imports. Solution: literals call `set.insert`, `set.empty`, `map.put`, and `map.empty` directly, still with `import set` and `import map`, and the generators of comprehensions take the fold of the type of their container, which is unique, and `*` goes.
3. **Within a module, overloading only distinguishes arities**, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Solution: overloading by type within a module as well, under the unique resolution of item 1, with the declarations told apart by their types in `self/defs.u`, `self/run.u`, and `self/compile.u`, e.g. by mangled names.
4. **Generic code over containers is impossible**, e.g. `function f[C](c: C) := size(c)`, nothing states what `size` must satisfy, and the hooks `compare`, `==`, and `show` are unchecked, with `Unordered` and `Incomparable` at run time. Deferred: named signatures, implemented by modules (`implements container with C[A] := Set[A]`), modules with parameters instantiated at the import (`import stats[set]`), one canonical implementation per type, and signatures `eq`, `ord`, and `show` that replace the hooks, where a missing implementation is a type error. Also open: laws as items of signatures, discharged once per implementing module. Until then, the hooks stay.

## Checker

1. **Failure is not in the types**: a procedure can raise any value without saying so, so a caller cannot see what may fail, and the checker cannot tell whether the handlers of a `try` are exhaustive. Solution: `raises E, …` on procedures, required in `export` and inferred inside a module, of declared datatypes only, by convention one error datatype per module. Every `raise`, and every call that can raise, is covered by a `try` or by the `raises` of the caller, the handlers are checked for exhaustiveness over the declared types, and procedure types carry their `raises` as written, without polymorphism over sets of exceptions. Open: how exceptions of functions outside their `requires` (Verification 1) count for the `raises` of a procedure.
2. **Termination is a style rule only**, so the split between functions and procedures guarantees nothing, and unfolding a function that does not terminate can prove false. Solution: the checker accepts recursion in functions only if it is structural on a parameter of a datatype, or decreases a measure declared by `decreases` after `:=`, whose decrease is an obligation of the prover, and otherwise reports an error that advises a procedure.
3. **Local `let`s are generalized**, which is rarely used and complicates inference once types carry more, such as `raises`. Solution: generalize only top-level declarations, which carry their types anyway.
4. **Locals may hide locals**, e.g. a pattern variable of a nested `match` or a formal of a lambda that hides a variable around it, a mistake that is not seen. Solution: an error when a formal, local, pattern variable, or formal of a lambda hides another of the same declaration, while locals may still hide declarations, since those depend on the imports.

## Syntax

1. **`else` followed by `if` always continues a chain of statements**, so a nested `if` cannot directly follow `else`. Solution: decide on a syntax that tells them apart.
2. **Accesses are applications of a variable `.f`** until the checker resolves them. Solution: a node `EField(e, f)` of the syntax.

## Library

1. **Two exceptions for indices**: `NoIndex(Str, Int)` of strings carries the string, whose text may be long, and `OutOfBounds(Int, Int)` of arrays the index and the size. Solution: `OutOfBounds` for strings too, which changes the messages of some error tests.
2. **The functions `forall(c, p)` and `exists(c, p)` of the containers** clash with the quantifiers of specifications (`prove.md`, open point 15). Solution: bounded quantifiers as syntax, `forall x in c => e` and `exists x in c if q => e`, with generators as in comprehensions, which code computes by the folds of the containers, and which specifications read as `forall x => contains(x, c) ==> e`, where the unbounded `forall x: T => e` occurs only in specifications, and the functions go. Open: the form for the entries of maps.

## Compiler

1. **Runtime errors have no positions.** Solution: positions in the messages of the runtime and of the interpreter, first for uncaught exceptions and failed `assert`s.
2. **Deep recursion**: functions like `concat` over long lists recurse deeply, so compiled programs need `ulimit -s unlimited` for big inputs. Solution: a large stack for the main thread of compiled programs, as the interpreter has.
3. **Programs need a C compiler**, and `cc -O2` takes most of the time of `make selfcompile`. Solution: a backend that writes assembly for x86-64, see `assembly.md`.

## Diagnostics

1. **Errors are lines of text**, which tools and agents parse. Solution: `--json` for `check`, `test`, and `run`, with a stable code per error, the file, line, and column, the expected and found types, and the qualified names involved.
2. **Errors do not suggest their fix** where the checker knows it. Solution: suggested edits, e.g. the missing cases of a match, or a qualified name for an ambiguous use.
3. **The resolution of names is invisible**. Solution: `ulang.bin check --resolved <file>`, and hover in the VS Code extension, showing the qualified name that each use resolved to.

## Verification

1. **Contracts, lemmas, and proofs** are not implemented, see `prove.md`, where functions have only `requires`, which states their domain, also that of their `raise`s and of the builtins, e.g. `requires d != 0` of `/`. Solution: parse specifications, lemmas, and proofs, and prove them with the simplifier of `prove/`.
2. **The simplifier of `prove/` reads its own syntax**, by `prove/read.u`, and does not see checked programs. Solution: a translation `self/prove.u` from the programs checked by `self/check.u`, with goals as `Bool` functions whose formals are the fixed variables, a default proof by simplification and induction on each formal of a datatype, where goals proven earlier are rules for later ones, and a command `ulang.bin prove <file>`, which needs the prover in `self/` or imports across directories, undecided.
3. **How strongly an obligation is established is not recorded.** Ideas, which need a design of their own: a status per obligation, proven, tested (Testing 2), or open; trusted facts by `assume … why "…"`; a command `ulang.bin audit <file>` that lists the assumptions and the obligations that depend on them; and runtime checks of the obligations that are not proven.

## Later

1. **Procedures may do anything that `os` offers**, invisibly in their signatures. Solution: a clause `uses os` on exported procedures.
2. **Failures of programs that read files or arguments are hard to reproduce.** Solution: `run` records the results of the `os_*` procedures and replays them.
3. **No native code of users.** Solution: `builtin` sections outside the library, with the native definitions linked in.

## Rejected and decided

- **A faster `self/run.u`** by numbering the names of declarations and variables when a program is loaded, so that the environments compare integers, and **integers tagged in the pointer** in the runtime, to save the boxes of most integers: both rejected.
- **Hash consing** of all constructors and tuples in the runtime, tried and removed (`be1808d` is the state without it): 70% of the constructions of the compiler and 85% of `run.u` equal a live value, and peak memory fell by 40–57%, but it was 2.2–2.5 times slower from cache misses, a bucket, its entry, and the candidate for every construction. **Maps and sets ordered by hash** were 18% slower for the compiler and 75% for `run.u`, since hashes outside -16..255 are boxed and `Name` keys differ early anyway. Consing particular structures, such as types or the terms of `prove/`, for memory or identity is still open.
- **A stack machine** for the interpreter, whose stack and code would be lists or maps, and **mutable arrays**, which values shared by two variables, atomic effects, and the restoring of variables by `try` exclude: rejected.
- **Partial field accesses** marked at the access, as `e!f`, or rejected statically: dropped, `MissingField` stays.
- **`call`, `assign`, and `skip`** stay, **`in`** stays at calls, **`test name()`** keeps its parentheses, and **`[…]`** stays for both type arguments and list literals.
- **User-defined notations** stay, although parsing depends on the imports.
- **Array literals** `|a, b|` stay; **`++` of arrays** is deferred, since arrays are built by comprehensions.
- **`Char`** stays a type of its own.
- **Functions may raise**, where `requires` states their domain (Verification 1).
