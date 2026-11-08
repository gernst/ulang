# ulang

A minimal language with pure functional expressions, imperative statements, datatypes, and Hindley-Milner inference, implemented in itself in `self/`: parser, checker, interpreter, and a compiler to C, with a runtime in C (`self/runtime/`). The Scala 3 implementation it started from is on the branch `scala-baseline`.

## Working rules

- A question asks for an answer, not an action: answer it, propose what suggests itself, and act only on an explicit go-ahead. Read-only lookups are fine.
- Checks:
  - After a refactor that keeps behaviour, `./ulang.bin check self/ulang.u` is enough.
  - Run `make test` after changes of behaviour or when asked, and say that it ran; after edits of tests, rerun only the affected files.
  - `make testc` only with a strong reason, e.g. changes to `self/compile.u`, the runtime, or what the generated C depends on, and only after asking.
  - Time every phase of a run of checks and keep the output, so that questions about durations are answered from it; never rerun only to measure.
- Commits: no `Co-Authored-By` for Claude; end the message with `Agentic Coding (<model>)`, e.g. `Agentic Coding (Claude Opus 5.5)`. Run `make` after every commit, so that `ulang.bin` is current.
- `TODO.md` holds open items only, each as the problem and the desired solution; delete an item when it is done. Explanations go into this file or `README.md`.
- Profile compiled programs with gprof, not perf, which cannot record call graphs here: build with `-pg`, run with `GLIBC_TUNABLES=glibc.gmon.maxarcs=50000000:glibc.gmon.minarcs=1000000`, and sum several runs by `gprof -s` if one has too few samples; gprof does not see time in shared libraries such as libgc.

## Build

- `make` builds `./ulang.bin` from `bootstrap/ulang.c`, the C of the last compiler that compiled itself to the same C, with the runtime, GMP, and the Boehm collector. `ULANG_LIB` is the directory `prelude/` of the library, for imports and `os_library`. Commands exit with status 1 if anything fails, and need `ulimit -s unlimited`, which the Makefile sets.
  - `ulang.bin run <file> [<procedure>] [-- args]` calls the procedure, by default `main`, without parameters, in the interpreter `self/run.u`; `os_args` gives the arguments after `--`.
  - `ulang.bin test <file> [<name> …] [-- args]` runs all or the named `test`s, each on its own, printing `test name passed` or `test name failed: …`.
  - `ulang.bin test <dir> [<path> …]` runs the `.u` files below the directories, in the order of names, and the given files, each in its directory with all its tests in one process. A file must succeed, or fail with the error that its first line states; it prints the files that do not, and `N of M files passed`.
  - `ulang.bin check <file>` only checks.
  - `ulang.bin compile <file> <entry> <target.c>` translates to C, whose `main()` calls the procedure `entry`, or runs all tests for the entry `test`; compile it by `cc -DULANG_LIB='"'$PWD/prelude'"' -I self/runtime target.c self/runtime/ulang.c -lgmp -lgc`.
  - `ulang.bin format <file>` prints the file as source (`format_program` of `self/format.u`), and fails if that does not parse to the same commands.
  - Parse errors are reported at their token, type errors at their declaration, as `at L.C` after the message, with ` in m` for an imported module `m` (`ImportError` of `self/parse.u`, `Located` of `self/check.u`). Compile and runtime errors have no positions.
- `make selfcompile` compiles the compiler with `ulang.bin`, and `make bootstrap` with the interpreter (`ulang.bin run self/ulang.u main -- compile …`). Both replace `bootstrap/ulang.c` and `ulang.bin` only if the new compiler compiles itself to the same C, and keep the previous ones in `bootstrap/<time stamp>/`, where the C is versioned and the binaries are not. `make selfcompile` takes about 35 seconds.
- `make test` is `ulang.bin test tests self prove prelude`, about 9 seconds.
- `make testformat` formats every file that parses, testing parser and printer by their round trip.
- `make testc` (`testc.sh`) runs the same files compiled, with the entry `test`, by `cc -O2` and the runtime object `out/testc/ulang.o`, in parallel, about 45 seconds. It skips `prelude.u` and `option.u`, which cannot be compiled on their own, since the runtime needs the lists of `list.u`.
- `make install` packages the VS Code extension of `vscode/` as `ulang.vsix` into VSCodium: highlighting, a run button, `▶ Run` links above procedures without parameters and tests, errors in the Problems view, and tests in the Testing view, by running the `ulang.bin` of the workspace or of the setting `ulang.binary`.

## Tests

- Tests are end-to-end only: programs in `tests/` with named tests `test name() := …` of `assert` statements. Names are distinct, and the parser accepts formals, which the checker rejects. Programs are entered by procedures without parameters, e.g. `procedure main() := …`.
- A file expected to fail states the error in its first line, e.g. `// type error: unknown variable x at 3.1`. They sit in the subdirectories of `tests/errors/` by kind of error, with helper modules next to the files that import them.

## Modules

- `import name` loads `name.u` from the directory of the parsed file, or else from the library, once, without its tests. The parser gives `Source(mods, main)` of `self/syntax.u`, the modules in load order, each after its imports, as `Mod(name, imports, exported, builtins, implemented)` with the commands of its sections paired with their positions. Its only state is the modules loaded so far, from which each module gets its initial notation, the exported notation of its imports (`exported_notation` of `self/notation.u`).
- Every module implicitly imports `prelude`, `option`, `list`, and `str` from the library, in that order, each of these only those before it (`implicit_imports` of `self/syntax.u`); a module may declare their names again.
- A module sees its own declarations, the exported ones of its direct imports, and the prelude, and likewise for notations: imports are not transitive. The checker resolves every name to its module, and checks each module once, after its imports, against their signatures (`check_program` of `self/check.u`, `Sig` of `self/sig.u`).
- Sections, see `prelude/set.u`: imports, then `export` with notations, datatypes, aliases, signatures without bodies, and abstract types `type T[A]`, then optionally `builtin`, then `implementation` with definitions that repeat the signatures exactly, `data` for the abstract types, private declarations, and tests. Other modules see neither the private declarations nor the constructors and fields of abstract types. A module without sections has all its commands implemented and exports nothing, so only main programs have no sections; their private procedures and tests are the entry points of `run` and `test`.
- A `builtin` section has exported signatures without definitions and types without a definition, `type T[A]`, which the interpreter (`builtin_names` of `self/run.u`) and the runtime (`builtin_functions` and `proc_name` of `self/compile.u`) define, make, order, compare, and show; its uses keep their module, and no `compare` or `==` is synthesized for its types. Examples: `str.u`, `os.u`, `array.u`, and the prelude, whose builtin section declares `Bool`, `Int`, `Str`, `Char`, the arithmetic `+`, `-`, `*`, `/`, `%`, and `++` of strings.

## Library

- The prelude declares the operators and the exceptions of builtins: `DivisionByZero` of `/` and `%`; `NoIndex(Str, Int)` of `at` and the strict `slice` of strings; `OutOfBounds(Int, Int)`, index and size, of `at`, `put`, and the strict `slice` of arrays; `NoCharacter(Int)` of `chr`; `MissingField(Str)` with the name `T.f`; and `Unordered(Str)` and `Incomparable(Str)` with the name of a type.
- `<`, `==`, `compare`, `show`, `and`, and `or` are builtins without a module. `compare` is a total order of all values, consistent with `==`, with results `-1`, `0`, `1`, where constructors are ordered by their position in their declaration; `<`, and the `<=`, `>`, `>=` of the prelude, each by one `<`, follow it.
- `str`: `list(s)`, `str(cs)`, `ord`, `chr`, `is_digit`, `is_letter`, `is_space`, `size`, `at(i, s)`, `get(i, s)` as an option, `contains(c, s)`, `slice`, `join`, `split`, `quote`, `number`.
- `map` and `set`: `Map[K, V]` and `Set[A]`, abstract types of AVL trees ordered by `compare`, with `compare` and `==` by their elements and entries; `intersect(s, t)` of sets.
- `array`: immutable `Array[A]`, kept as `VArr` of `Value` in `self/run.u`: `array(xs)`, `list(a)`, `size`, `at(i, a)`, `get(i, a)`, `slice`, `contains`, and the folds.
- Lists, sets, maps, arrays, and strings share names and orders of arguments:
  - `size`, `contains`, `get` (of association lists for `list`), `remove` (elements of lists and sets, keys of maps), `empty` of sets and maps, `exists(c, p)`, `forall(c, p)`, and the folds `foldr(c, acc, f)` and `foldl(c, acc, f)`, in the order of lists and ascending for sets and maps, where `f` and `p` of maps take key and value.
  - Conversions are named after their target and take a list: `list(c)` of the elements or entries in order, `set(xs)`, `map(ps)`, where the last entry of a key counts, `array(xs)`, `str(cs)`.
  - `infix right 8 ++` and `infix left 7 --`: `++` concatenates lists and strings, unites sets, and joins maps, where the entries of the second count; `--` removes the elements of a list or set, and the keys of a set from a map.
- `os` declares `OsError` and the builtin procedures `os_args`, `os_read`, `os_write`, `os_library`, `os_print`, `os_print_error`, `os_exit`, `os_list` (sorted entries with whether each is a directory, without following links), `os_is_dir`, `os_cwd`, and `os_chdir`, which raise `OsError` when they fail on files, with `os_walk` of `os.u` for the files below a directory, and relative paths start from the working directory.

## Overloading

- The candidates of a name are its declarations in all visible modules; locals hide them all, a qualified name `set.size` has one, and within a module, overloading is only by arity.
- An applied name infers its arguments first, then keeps the candidates of its arity whose instance unifies with them and the expected type, each tried on a copy of the substitution. One remaining candidate is committed; with several, the use gets a fresh type, a placeholder name, and a pending entry.
- Pending entries are solved to a fixpoint at the end of the command, before `zonk` replaces the placeholders, and lets do not generalize their types. Of several that fit, the checker prefers the most specific for a known type, the most general for a type still unknown (so the builtin `==` serves where no declaration fits), a single one of the module itself, and else the one after which the other open uses still have candidates, e.g. `map.empty` and `map.put` of a map literal when `set` and `array` are imported too. No candidate is an error, several are ambiguous, both listing the candidates with their types.
- Accesses `e.f` and updates `e with f := a` whose `e` has no known type yet are pending entries with the accessors `T.f` of the visible datatypes as candidates, so `lambda p => p.x` checks once `p` is used.

## Syntax and translations

- Every argument of a constructor is a named field, `C(name: T, …)`; a name may repeat in the constructors of a datatype with the same type, and patterns bind the variables of the fields' names where they can. Fields give accessors `e.name` and updates `e with name := a, other := b`, parsed as `EField` and `EUpdate` (from the left) and resolved by the type of `e` to the generated `T.name` and `T.name:=`, so that checked programs have neither, which raise `MissingField("T.name")` for constructors without the field. A comma followed by `name :=` continues the updates, also in `assign` and `local`.
- Literals of sets and maps are unrolled folds: `{a, b}` is `insert(a, insert(b, empty()))`, `{k => v, l => w}` is `put(l, w, put(k, v, empty()))`, `{}` is `empty()`, and a rest after `;` is added to, `{a; s}`, `{k => v; m}`, as `[a; xs]` for lists.
- Comprehensions are folds over the containers of their generators, nested for several: `[e for x in c if q]` is `foldr(c, Nil, lambda x, acc => if q then Cons(e, acc) else acc)`, `{e for x in c if q}` the same with `empty()` and `insert`, and `{k1 => v1 for k => v in m if q}` is `foldl(m, empty(), lambda acc, k, v => if q then put(k1, v1, acc) else acc)`. The accumulator has a name users cannot write.
- `insert`, `put`, `empty`, `foldr`, `foldl`, and `array` of these translations are names of the module `*`, which resolve against the visible declarations but never against locals (`overloads` of `self/check.u`), so literals and comprehensions of sets and maps need `import set` and `import map`.
- `|a, b|` and `|e for x in c if q|` are `array` of `*` applied to the list or list comprehension, and `show` gives `|a, b|`. The lexer reads `|` alone as punctuation, so no operator contains it; a `|` where an operand starts opens an array, and the next `|` after an operand closes it.
- `-n` for a number `n` is a negative literal, in expressions and patterns, e.g. `case -1 =>`.
- `self/compile.u` builds list comprehensions over lists and arrays, also of several generators, by nested loops without intermediate lists, array literals by `u_mk_array`, and array comprehensions by loops that fill an array sized by the first generator, grown for further generators and shrunk at the end unless it is exact.

## Semantics

- `raise e` is an expression of any type, and a failed `assert` cannot be caught.
- Effects on variables are atomic: a procedure works on copies of its outs and refs, which reach the caller only when it returns, and the handlers of a `try` see the variables as before it. Compiled code copies outs and refs in and out, and saves the variables that the body of a `try` assigns before its `setjmp`.
- Variables are read only where they are assigned on every path, and outs must be assigned on every path that does not raise, where passing a ref reads it (`Local` of `self/check.u`, joining the paths of `if`, `match`, and `try`; a `while` may not run its body, and a handler starts from the variables before its `try`).
- Procedures are values of types `(in A, out B, ref C)`: declared ones by name, and `lambda in x, out y, ref z => statements end`, which sees the variables around it by their values and assigns only its own outs and refs. `call p(…)` calls a variable `p`, which hides a procedure `p`; compiled code calls through `u_call_proc` and the wrappers `wp_`.
- Functions without parameters are computed once, interpreted and compiled.
- Orders, equality, and show of datatypes:
  - A function `compare` or `==` of two values `T[A, …]` of a datatype `T` of its own module, with distinct type parameters, is the order or equality of `T` (`order_of` of `self/defs.u`), which the builtins `compare`, `==`, and `<` use for all values of `T`, also inside other values and in generic code; other datatypes compare structurally.
  - Only abstract types declare `==`; datatypes with visible constructors may declare `compare`, which must agree with structural equality, unchecked. With `compare` alone, `==` is `compare(a, b) == 0`.
  - For an abstract type without `compare`, the checker synthesizes a private `T.compare` that raises `Unordered`, and without `==` either, a `T.==`, both raising `Incomparable` (`tests/orders.u`, `tests/modules.u`).
  - A function `show` of one value `T[A, …]` of a datatype `T` of its own module, with distinct type parameters and the result `Str`, is the show of `T`, which the builtin `show` uses for all values of `T`, also inside other values; otherwise `show` gives the text of a value, strings in quotes.
  - The runtime finds these through the type descriptor of each object (`struct u_type` of `self/runtime/ulang.h`, one `t_` per datatype, pointing to the declared functions or the structural ones), the interpreter through the maps of `Defs` of `self/defs.u` from datatypes to them.

## Style

- Formals are the parameters of functions, procedures, and lambdas; params are type parameters.
- Omit comments that are self-evident from the code, such as doc comments that restate the name of a declaration.
- Functions only for computations that terminate; code that may not terminate, such as the evaluator of `self/run.u`, is written as procedures, unchecked.
- Functions, procedures, and locals are snake_case, e.g. `zonk_type`; constructors and types are CamelCase, e.g. `TSort`, `TypeError`.
- Names are descriptive but short, and one concept has one name, without short and long forms: `Constr`, `Mod`, `Sig`, `Prog`, not `Constructor`, `Module`, `Ctx`, `Program`.
- Variables are named by their type: `toks`/`tok`, `cmds`/`cmd`, `sig`, `loader`, `run`, `typ` and `types`, `fun`/`funs`, `text` for source and `offset` for positions in it, `name` for an `Id`, `cases`, `cond`, `body`, `prec`, `constr`/`constrs`, `field`/`fields`, `prog`, `expr`/`exprs`, `pat`/`pats`; `i`, `j` for plain counters.
- Procedures that consume tokens start with `parse_`, except `advance` and `expect`; functions that build trees do not. Predicates on the next token are `at_X`, grouped together.
- `show` is text that parses back literally, never `show_X`; `format_X` prints into another language, e.g. types, values, tokens, programs, or C as source.
- The notation is threaded as the first parameter where it is needed, not bundled into records.
- Signatures that do not fit on one line have one parameter per line, with `in  ` padded so that names align with `out ` and `ref `.
- Write recursions with a large `match` explicitly, not as `foldl` with a step function; folds are for small steps.
- In traversals such as `prove/simp.u`, bind every result of a recursive call by `let` before using it, keep the recursion in the traversal (`simp_expr`), which passes the simplified parts to one helper per kind of term (`simp_app`, `simp_ite`) that only combines them, and put each case body on the line after `=>`, with blank lines between cases.
