# The implementation

ulang is implemented in itself in `ulang/`: lexer and parser, printer, checker, interpreter, prover, lints, refactorings, and compilers to C and to x86-64 assembly, with a runtime in C (`ulang/runtime/`) and the library in `ulang/prelude/`. The Scala 3 implementation it started from is on the branch `scala-baseline`. This file is the overview: the phases, the data passed between them, and the invariants that cross them. The detail of each phase is in `doc/implementation-notes.md`, the commands in `doc/tool.md`, and the language in `doc/language.md`.

## Phases and packages

Each phase is a package of its own, a directory, which imports those before it, so the imports check the layers:

| Package | Owns |
|---|---|
| `ulang/syntax` | the tree that all phases share (`Mod`, `Cmd`, `Expr`, `Stmt`, `Pat`, `At`, `Prog`), the builtins (`ulang/syntax/builtins.u`), the notation of operators (`ulang/syntax/notation.u`), and the errors (`ulang/syntax/error.u`) |
| `ulang/format` | the printer, `format_program` of `ulang/format/format.u`, which writes the only accepted layout and puts the comments back |
| `ulang/parse` | the lexer `ulang/parse/lex.u` and the parser `ulang/parse/parse.u`, which loads modules and packages into a `Source` |
| `ulang/expand` | the expansions of the constructs that the checked program keeps as the source wrote them, into the smaller language that the backends translate, by which the checker also builds the translations that it types forms by (`expanded` of `ulang/expand/expand.u`) |
| `ulang/check` | signatures (`ulang/check/sig.u`), definitions and hooks (`ulang/check/defs.u`), the checker (`ulang/check/check.u`), the exceptions of procedures (`ulang/check/raises.u`), the uses of imports (`ulang/check/uses.u`), the hints (`ulang/check/hints.u`), and the messages of type errors (`ulang/check/message.u`) |
| `ulang/run` | the interpreter, `ulang/run/run.u` |
| `ulang/compile` | C and the compiler to it (`ulang/compile/c.u`, `ulang/compile/compile.u`), x86-64 assembly and the compiler to it (`ulang/compile/asm.u`, `ulang/compile/x86.u`, `ulang/compile/elf.u`), and the part they share (`ulang/compile/lower.u`) |
| `ulang/prove` | the prover by contextual simplification over checked programs: translation, simplifier, e-graph, termination, proofs, and lemmas |
| `ulang/lint` | the lints of `check --lint`, walks of the parsed module (`ulang/lint/lint.u`) |
| `ulang/refactor` | the scripted edits of `refactor` (`ulang/refactor/refactor.u`, `ulang/refactor/nodes.u`, `ulang/refactor/habits.u`) |
| `ulang/test` | the tester, the files at the top of `ulang/test/`: the tests of lemmas and properties that it generates (`ulang/test/generate.u`), the enumeration of values (`ulang/test/enumerate/`), and the running of tests (`ulang/test/runner.u`); its subdirectories hold the tests of the language |

The driver `ulang/ulang.u` is a program that imports them all but `ulang/prove`, which it reaches through `ulang/test`. Beside them:

- `ulang/runtime/` is the runtime of compiled programs, `ulang/runtime/ulang.c` and `ulang/runtime/ulang.h`, with GMP for integers and the Boehm collector, and `ulang/runtime/window.c` for windows.
- `ulang/prelude/` is the library, the package `prelude`, which every module sees.
- `bootstrap/ulang.c` is the seed, the C of the last compiler that compiled itself to the same C, from which `make` builds the tool (Building of `doc/tool.md`).
- `checks/` holds the checks of the repository, programs that run compiled at the root, such as `checks/docs.u`.
- `library/` holds packages that programs import, such as `library.regex`, `examples/` programs, `gui/` a layer of Elm over windows, and `vscode/` the extension for VS Code and VSCodium.

## The data between the phases

- **Source.** The parser gives `Source(mods, packages, main)` of `ulang/syntax/syntax.u`: the modules in load order, each after its imports, as `Mod(name, imports, exported, builtins, implemented, comments)`, with the commands of its sections paired with their positions and the comments of the file, the packages as `Package(id, members)`, and the module of the loaded file.
- **Prog.** The checker gives `Prog(cmds, builtins, imports, main)`: every command of all modules with qualified names, the definitions checked and the rest as written, then the tests of examples and lemmas, the builtin sections apart, and the imports of each module at their lines, which hold the errors of imports. The backends and `translate` of `ulang/prove/translate.u` take `definitions`, without the signatures and the pattern aliases.
- **Names.** A resolved name is `Name(path, base)`, where the path is the module from the root of the project, shown with `.`, `ulang.check.check`, or `prelude.map` for the library; the names that the checker makes, accessors `T.f`, updaters `T.f:=`, and the tests `f.example.N` and `l.lemma`, are `Owned` names of their owners.
- **Sig and Defs.** `Sig` of `ulang/check/sig.u` holds what a package declares, its types, constructors, values, pattern aliases, and lemmas, behind the functions `with_*`, `*_of`, and `is_*`, which keep the layout of its maps private; `Defs` of `ulang/check/defs.u` holds what the backends need of a program: the definitions, the positions of the fields in each constructor, and the hooks of each datatype.
- **Parts.** Every part of an expression, statement, and pattern is an `At`: `AOk(pos, span, that)`, `AUnparsed(pos, span, text, error)` of a part that does not parse, or `AErr(pos, span, that, errors)` of a part or command that checking rejects, with its diagnostics.
- **Diagnostics.** Each error is a `Diagnostic` of `ulang/syntax/error.u`, with its file, module, position, kind, code, message, details, and severity, printed as one JSON line, the only output of errors, which agents and the VS Code extension read. Runtime errors are `RuntimeError` of `ulang/syntax/error.u`, whose texts both backends share, and the errors that checked programs never raise are `InternalError`.

## Invariants across the phases

### Constructs stay as written until expand

The parser desugars nothing: `for` statements, lambdas with patterns, the forms, accesses and updates of fields, `let` of patterns, pattern and type aliases, alternatives, and ascriptions stay as written, so that `format` prints what the source wrote and errors are reported against it. The checker resolves names, accesses and updates of fields, and type aliases, and keeps the other constructs as written, with what their expansions need recorded, such as the type of the container of a generator or of a formal that is a pattern, while checks such as exhaustiveness expand them internally. So the hints, the raises, and the lints read the constructs, not their translations. The backends, the interpreter, the compilers, and `translate` of the prover, each take `expanded` of `ulang/expand/expand.u` of the checked program first, which gives them a smaller language: folds instead of forms and generators, constructors instead of pattern aliases, lambdas that bind only variables, one binding per `let`, and ordinary locals instead of the outs `_`.

### Comments are not in the tree

As in gofmt and rustfmt, the lexer gives every line of a comment as a `Comment` with its position, which the parsed module holds in `Mod.comments`, while the tokens and the parts have none, so the parse and the phases after it do not depend on them. `format` puts each back by the spans of the parts, above the part it precedes or at the end of the line of the part before it, and `refactor` splices the text of whole declarations and nodes by those spans, so no tool loses or moves a comment. The doc of a declaration is the run of comments right above its span.

### Positions and spans

Every part has its position, the first token or the token that names it, a call at its name, an operator at the operator, and its span from its first token to the end of its last, which the parser reads from the tokens. The translations of the checker take the span of what they translate, and code that a phase makes without one the empty span at its position. Positions are never stripped: the checked program keeps them, code that does not care reads `x.that`, and comparisons modulo positions, as the round trip of `format`, set them to `Pos(0, 0)`. A part counts for the hints only if the parser placed it, a name written over its span. Parse errors are reported at their token, type errors at the innermost part around them.

### Errors are data, and deferred

- The parser resumes after an error at the next command, statement, or argument, and holds each part that does not parse as an `AUnparsed`, without depending on the layout, only on the tokens.
- The checker records each error by `report` and continues: a part that does not parse, an unknown name, and a call that no declaration fits have the type `TError`, which unifies with every type and makes no follow-up errors, and a part, when its check ends, takes the errors reported while it was checked that none of its parts took, as an `AErr`, while errors that manifest after the parts, of pending overloads, declarations, and raises, stand on the import or the command. The walk of errors, `cmd_errors` and `part_errors` of `ulang/syntax/syntax.u`, is one for the source and the checked program.
- No phase stops at an erroneous part: each treats it as it treats a part that does not parse. Each package is checked against the signatures of those before it, also of those with errors, and only a package whose check raised stops the packages after it.
- `run` and `check --test` run a program with errors if its main module is checked: a definition that its own errors make unsafe to run has an error node as its body, and the interpreter raises the runtime error `Deferred` where it reaches one, so only the paths through such a part raise. `check --prove` proves no lemma if code that the lemmas may reach has errors, and `compile` refuses errors other than those that generate no code.
- The status of a command is the worst of its parts: 0, 1 for errors of the sources, 2 for usage, 3 for a failed run, test, or lemma, 4 for an internal error.

### Modules, packages, and the cut load

- The root of a project is the nearest directory above a file with `project.u`, else the file's own directory, and a module is known by its path from the root without `.u`.
- Packages: every `.u` file but `project.u` is a member of the package of its directory, known by the directory from the root, and `prelude` for the library, also a file without sections, which has all its commands implemented and exports nothing. A package is exactly its directory, as in Go, spanning no subdirectories, so that the imports check the layers of `ulang/`, which a package of its subdirectories would merge, and each directory of tests below `ulang/test/` is a package of its own. Loading a module loads its package, all members. Members see each other's exports without imports, also cyclically, and overload each other by type, while their private declarations, also their procedures without formals and their tests, and the constructors and fields of their abstract types stay private to their file; several members may each have a private `main`, which `run file.u:proc` chooses by the file. Packages are the only units of imports, and cycles are cycles of packages. A project with a directory `prelude/` at its root shares the ids of the library, which `import prelude` rejects, and clashes with it in C.
- Every module implicitly imports the whole library, the package `prelude` of `prelude`, `option`, `list`, `str`, `set`, `map`, `array`, `buffer`, `os`, `io`, and `window`, so no module imports one; a module may declare their functions again with other types or arities, which overload them, and their types, which hide none, as no type hides another, of the library, of an import, or of the module itself, so that two visible types of one name make an unqualified use ambiguous, and the use writes the path of the one it means; each member that names a type of an imported package imports that package itself.
- A module sees its own declarations, the exported ones of the members of the packages it imports and of its own package, and the prelude, and likewise for notations: imports are not transitive. The checker checks the members of each package together, once, after their imports, against their signatures, and an import of which the module uses nothing is `UnusedImport`.
- Sections, see `ulang/prelude/set.u`: imports, then `export` with notations, datatypes, aliases, signatures without bodies, and abstract types `type T[A]`, then optionally `builtin`, then `implementation` with definitions that repeat the signatures exactly, `data` for the abstract types, private declarations, and tests. Other modules see neither the private declarations nor the constructors and fields of abstract types. A module without sections has all its commands implemented and exports nothing; its private procedures and tests are the entry points of `run` and `test`. A `builtin` section has exported signatures without definitions and types without a definition, which the interpreter and the runtime define.
- The cut load: the checks that report only the errors and hints of the package of the file, `check` without `--test` and `--prove`, `hints`, and the checks of `refactor`, load the other packages, the library included, only as far as their importers see them: the parser cuts each member to its imports, its export and builtin sections, and the `data` and `type` commands of its implementation, and the checker only declares those packages, their exported functions and procedures by their signatures. The signatures that the importers see, the diagnostics of the file, and its hints must be those of the whole load, while the errors of the definitions of the imports are not found; the tests of `ulang/test/declared/` check this on a package of each kind of declaration.

### Overloading resolves uniquely

The candidates of a name are its declarations in all visible modules, its own module's no more than the others, and locals hide them all; within a module, a name is overloaded only by arity. An applied name keeps the candidates of its arity whose instance unifies with its arguments and the expected type; one remaining candidate is committed, and several leave a pending entry, solved to a fixpoint at the end of the command. Since the substitution only grows, a candidate that does not fit never fits again, so a use resolves by its types alone, without preferences and whatever the order of solving; no candidate is an error, and several at the end are ambiguous, both listing the candidates with their types. Each command starts with a fresh substitution and numbering of unknowns (`enter` of `ulang/check/check.u`), and is solved and zonked at its end, so a type that a phase records while one command is checked must be zonked before the next. Lets do not generalize. Accesses of fields resolve by the type of the expression, which must be known when the checker reaches them, and lambdas that are arguments wait until the type of the function or procedure is known.

### Raises are inferred and checked after the package

Functions do not raise and fail only by the exceptions of builtins and accessors, which no `raises` counts. What a procedure may raise is checked after its package, if it has no other errors, by `raises_errors` of `ulang/check/raises.u`: exported procedures, entries without formals, and procedures with `raises` raise only what they declare, while private procedures with formals and procedure values bound without a type have their raises inferred as the least fixpoint, which the checked program then carries, so that the hints show them and no unknown reaches a later phase. Procedure types carry their raises, with subtyping by the direction in which values flow, and without polymorphism over sets; a `try` removes the types that its unguarded handlers cover, and a handler of a type that its body cannot raise is an error.

### Effects are atomic

A procedure works on copies of its outs and refs, which reach the caller only when it returns, and the handlers of a `try` see the variables as before it; the assignments of one statement and the declarations of one `let` or `var` are sequential. The interpreter and both backends implement this alike: compiled code copies outs and refs in and out and saves the variables that the body of a `try` assigns in the slots of its handler, never in locals of C or in registers across a `setjmp`.

### Orders, equality, and show

`==`, `compare`, and `show` are builtins without a module, structural for every value, with `compare` a total order of all values, constructors ordered by their position in their declaration. A datatype may declare its own `compare`, `==`, or `show` in its module, a hook of a fixed form (`Hook` of `ulang/check/defs.u`), which the builtins then use for all its values, also inside other values and in generic code; declared hooks are no candidates of overloading. Abstract types without a `compare` raise `Unordered`, and without `==` either `Incomparable`. The runtime finds the hooks through the type descriptor of each object, the interpreter through the maps of `Defs`.

### Specifications become tests

The checker translates each example of a function or procedure into a test `f.example.N` of its module, checked as written; the tester generates a test `l.lemma` for each lemma whose formals have enumerable types, and one for each test with formals, which run them on all values of the formals up to a small depth and report the smallest counterexample. Only `check --prove` decides a lemma, by the prover, over the checked program translated by `translate` of `ulang/prove/translate.u`; a recursive function unfolds there only once its termination is proven, and a claim `correct p(…)` is proven from the contracts of the procedures that `p` calls and an invariant per loop. Imports drop the examples and the lemmas of the modules they load.

### The backends agree

The interpreter, the C backend, and the assembly backend give the same outcome for every program, with the texts of shared runtime errors in one place, `runtime_message` of `ulang/syntax/error.u`; `make testc` and `make testelf` run the tests of the project in the compiled backends, against the interpreted `make test`. The order in which a comprehension evaluates its parts is the one exception, unspecified.

## What to keep

These parts of the implementation are right, and later changes should not weaken them:

- **Phases as packages whose imports check the layers**, each importing only those before it, and the driver `ulang/ulang.u` all of them.
- **Errors as data**: diagnostics are values with positions, recorded by `report` of `ulang/check/check.u` while the check continues, and kept in the checked program as error nodes `AErr`, which each later phase treats as a part that does not parse; each command starts with a fresh substitution and numbering of unknowns (`enter`), and is solved and zonked at its end.
- **Narrow interfaces**: the functions `with_*`, `*_of`, and `is_*` of `Sig` keep the layout of its maps private.
- **Tests across components**: the printer by its round trip through the parser, which every file that checks passes; the interpreter against compiled code (`make testc`); the errors that a phase must raise as cases of `ulang/test/errors/`, each expecting the exception and its value from a run; and the references of the documentation by `checks/docs.u`.
- **Bootstrap**: the seed is the C of the last compiler that compiled itself to the same C, so a change that breaks the compiler cannot replace it.
- **Backends**: C as a small typed syntax with a printer that adds parentheses (`ulang/compile/c.u`), and x86-64 assembly by instructions as data (`ulang/compile/asm.u`), not built from strings, with the analyses that both share in `ulang/compile/lower.u`; one type descriptor per datatype with `compare`, `equals`, and `show`, mirrored by the maps of `Defs`; outs and refs copied in and out alike in all backends; and fused comprehensions that fall back to the general fold.
- **The prover**: terms with de Bruijn indices for binders and levels for the variables of open sequents, with one `rebind` and its invariant written down; an e-graph with congruence closure; canonical sums with Fourier–Motzkin elimination and integer tightening; named limits (`Limits` of `ulang/prove/simp.u`); recursive functions unfolded only once their termination is proven; and end-to-end tests that include the goals expected to stay open.

## Where to read more

`doc/implementation-notes.md` has the detail of each package, with the functions that decide it: for the lexer and the parser, their recovery and the loading of packages; for the checker, fields, forms and generators, patterns and alternatives, locals, overloading, imports and the cut load, examples, lemmas, contracts and claims, raises, hooks, and its errors; the expansions; the interpreter; both compilers and the runtime; the prover; the tester; and the library.
