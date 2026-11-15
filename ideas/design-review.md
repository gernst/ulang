# Design review

A review of the design of ulang as an agent-first language, as of 5cd2fe1: what holds, what does not, the trip-ups that `unclear.md` collected until then, sorted by what they show, and a judgement of every item of `ideas/`, whether it is needed and in which order. Delete this file when its proposals are taken up or rejected, each into the item of its area or into `ideas/rejected.md`.

## 1. What holds

The semantic core suits writers that make plausible mistakes and readers that are machines, and later changes should not weaken it (`DESIGN.md`, What to keep):

- **Pure functions and procedures with modes at every call**: the data flow of `p(in a, out b, ref c)` reads without the callee; this does more for the cost of review than any other feature.
- **Atomic effects under exceptions**: a handler sees the variables as before its `try`, so no review asks how far a body got, and backtracking needs no undo code.
- **Errors as data**: JSON lines at the innermost part, recovery at statements and arguments, `TError` without follow-ups, and programs with errors that still run as far as they can. The checker is how an agent learns the language, and this is the part that takes that seriously.
- **A hint for every departure from a habit** (principle 10), the right answer to having no training data, and a test of arbitrariness: a rule without a hint is suspect.
- **Overloading without preferences**, one canonical layout enforced by `check`, deterministic containers and enumeration, and a ladder of evidence, from examples through tested lemmas to proofs, each optional.
- **`ideas/rejected.md`**, which keeps decisions from coming back.

## 2. What does not

Each finding with the evidence of `unclear.md` (section 3) where it has some, and a proposal.

1. **The documentation fails principle 5.** `CLAUDE.md` is about 15,000 words, sentences of 150 words with chains of "`x` of `y` of `z`", mixing the language, the implementation, and the workflow, loaded into every session; three trip-ups are about keywords, which no place lists. Proposal: Documentation 1 first, in progress on the branch `docsplit`; the reference begins with the grammar and the list of keywords, and states each rule by an example that runs.
2. **Prose drifts, and prose is the only training data.** `DESIGN.md` described comments as part of the tree since 8cdf304 took them out, and the prelude still described lists as `[a, b; xs]`, sets as `{a, b}`, and arrays as `|a, b|`, both fixed with this file. `checks/docs.u` checks that the names exist, not that the claims hold. Proposal: claims about values become examples; a check that the sources contain no syntax that the lexer rejects inside comments, such as `[a, b]` outside type arguments, would have caught the second.
3. **Overloading across modules works against locality.** Which `size` a call takes depends on the imports and the types, so an example written on `set.size` silently tests `list.size`, `empty()` as the accumulator of a `foldl` is ambiguous among three modules, and `UnknownFieldOwner` asks for a type that cannot be written in an expression. Proposal: Modules 2 (qualifiers) and Syntax 5 (ascription) are the two fixes, both needed, and Diagnostics 3 (`sig` and `resolve`) makes the resolution visible before it surprises.
4. **Keywords that are common English words**, `pattern`, `example`, `test`, `data`, `raises`, `requires`, `ensures`, `correct`, `invariant`, and `induction`, natural names of fields and locals, which tripped agents five times. Proposal: those that occur only at fixed places, `correct`, `invariant`, `induction`, `requires`, and `ensures` after `:=`, `pattern` and `example` at the start of a command or line of a spec, become contextual; the rest get a hint with a name to write instead, `raised` for `raises`, `cmd` for `test`.
5. **Defects that fail only at run time**: `MissingField` of an accessor on a constructor without the field, e.g. `xs.head` of `List()`; `Unordered` and `Incomparable` of abstract types; and hooks `compare` that must agree with structural equality unchecked. Principle 8 targets exactly these plausible mistakes. Proposal: reconsider the rejected partial accesses for the agent case as a lint first, an access `e.f` where the type of `e` has constructors without `f` and no enclosing `match` narrows it, counted over the project before it is proposed.
6. **The order of arguments is not uniform**: `take(n, xs)` and `split_at` count first, while `slice` takes the container first, and `get(k, m)` and `contains(x, c)` take the element first, so an agent guesses wrong where the types agree (`Int` and `Int`) or the checker reports a mismatch far from the cause. Proposal: one rule, the container first, as `exists`, `forall`, and the folds have it, with the old orders as hints; or at least an example on each such signature, which the review of c319a1d asks for.
7. **Inconsistent heads**: lemmas take modes, `lemma l(in x: Int)`, while tests with formals take none, `test t(xs: List[Int])`, and an example on an exported signature needs a `:=` that a signature otherwise never has. Proposal: one form for both heads, and the error at `example` after a signature hints at the `:=`.
8. **Simultaneous assignment**: in `a := …, b := f(a)` the `f(a)` reads the `a` from before, the opposite of the sequential `let`, which most agents will assume for both. Proposal: a lint where a later assignment of one statement reads a variable that an earlier one assigns.
9. **Slow feedback**: the interpreter is about 300 times slower than compiled code, a case of `ulang/test/errors/` takes 12 to 30 s alone, a test of `ulang/ulang.u` 17 to 46 s, and `make test` about 65 s. The decision against a faster interpreter is sound, so the loop of an agent needs `check --test-compiled` of Build 1, which deserves a place before most of the diagnostics.
10. **The surface is large for a minimal language**: a prover with an e-graph, contracts and claims, two native backends with an encoder of ELF, lints, a planned refactoring tool and language server, and raises with subtyping and variance. By principle 5 the complexity sits in positions and errors, examples, the forms, contracts, hooks, and raises. Proposal: the critique of section 4 cuts or parks what an agent writing programs does not meet; the messages of raises, `WiderRaise` and `UntypedRaise`, get cases that show they name the value and the place to change, else agents patch them by trial.
11. **Small breaches of the own conventions**, such as `ys := ys ++ List(stmt)` in `statements` of `ulang/check/check.u`, where a list grows with the program: Diagnostics 5 (appends in loops) catches them.

## 3. The trip-ups of `unclear.md`

All entries of `unclear.md` as of 5cd2fe1, which leaves it empty but for its header. Each is in one of four groups.

**Evidence of section 2**, taken up by its proposals:

- Keywords as names: `data` as a formal, `pattern` as a variable, `test` as a field, `raises` as a local (2.4).
- `take(alts, 1)` against `take(n, xs)` (2.6).
- Lemmas take modes, tests with formals do not; an example on an exported signature needs `:=` (2.7).
- `empty()` as the accumulator of a `foldl` ambiguous among `buffer`, `map`, and `set` (2.3).
- A single test of `ulang/ulang.u` takes 17 to 46 s, a case of `ulang/test/errors/` 12 to 30 s interpreted (2.9).

**Gaps of the diagnostics**, for Diagnostics 6 and the hints of principle 10:

- A constructor applied to one argument too few is `cannot unify (Pos, Span, ?15) -> At[?15] and …` in an expression, while the same in a pattern names the arity, followed by `the pattern expects 1 components, got 0` and an `unreachable case` with a `_` that the source does not have: arity of constructors in expressions, and no follow-ups of a pattern of the wrong arity (add to Diagnostics 6).
- A call with one in too many gives `expects 5 ins, got 6`, then `expects 0 values, got 1`, and a mismatch per argument after the extra one, shifted by one (Diagnostics 6, the wrong count at a call).
- `binding.first.pos` is `UnknownFieldType` at `.first`, since tuples have no fields: a habit that deserves a hint, `first(binding)` or a pattern.
- `List(for _ => h in m)` needs `yield h`, since a pattern with `_` is no value, which "yields what its pattern binds" does not make plain: the message could say so, or `yield` could be omitted where the pattern binds exactly one variable.
- A parse error in the head of a procedure skips the whole command, so an error in its body is reported only after the first is fixed: the parser could resume inside the body of a head that does not parse.

**Facts for the documentation**, for the reference of Documentation 1 or for `CLAUDE.md` once the branch `docsplit` is merged:

- A hint of `hint_of` of `ulang/check/message.u` is no `Hinted` of `ulang/check/check.u`, so a case reads such a hint by `located_message`.
- A case of `ulang/test/errors/` runs alone in the interpreter, `check --test ulang/test/errors/types.u:<name>`; only the walk of `check --test` skips the directory.
- The message of `OpenLemma` of a gap ends with ` at L.C`, which only the cases of `ulang/test/errors/lemmas.u` show.
- `Seq` and `Concl` are declared in `ulang/prove/simp.u`, not with the terms in `ulang/prove/syntax.u` (or move them, Code 4).
- Moving a module out of the library keeps its file until `make selfcompile`, since the seed reads the library modules it has compiled in.
- `out/ulang.sh run` and `check` are the built binary, so an edit of the checker shows only after `make`; and `check ulang/ulang.u` does not run the tests of the members of the packages it imports, which imports drop, so the rule that it suffices after a refactor that keeps behaviour misses tests that build the changed datatypes: the rule should name `check --test` of the changed files too.
- Every `.u` file is a member of the package of its directory, so a scratch file or a set of texts of cases in one directory are checked together: each needs a directory of its own.
- The `Int`s from -16 to 255 exist once in the runtime, every other one allocates, so `(n % 256 + 256) % 256` allocates where `n % 256` does not.
- The binary reads the library from the directory compiled in, so comparing the output of two compilers also compares two libraries: take the reference after a change of `ulang/prelude/`.
- Scripted rewrites by text are hazardous: a rewrite of tuples by their shape also hit pairs in a `match` of the parser, a rewrite of the field `at` also hit strings and the field of another record, which only a run caught, and a pattern matched against the text before `format` missed after it. Each is a case for the refactorings of Diagnostics 4, and meanwhile a script asserts that each pattern matches exactly once.

**Done or obsolete**, dropped: a doc above the body of a function, and a doc after a `local` line that the seed rejected, both before comments left the tree (8cdf304) and `local` became `let` and `var`; the filter of `targets_of`, a note on the review; the order of loading the enumeration, now `load_after_library` as `CLAUDE.md` says; the spans of the formals of checked lambdas, fixed; definitions of a package whose check raised, now error nodes; the `_` in directories of imported packages, which `CLAUDE.md` says; tuple types written `A * B` and a call of a procedure without its mode, whose hints work; the round trips of comments tested in `ulang/ulang.u`, a decision taken; `drop_while` and `take_while`, which are Library 2; and the renaming of `test` to `check --test`.

## 4. The items of `ideas/`, judged

For each item: keep, with its rank, cut to a part, park until a condition, merge, or drop. Ranks by the value for an agent writing programs, `A` first, `C` last.

### Diagnostics

1. **Merge into item 4**: the text of an unparsed part is needed only by refactorings and the language server.
2. **Merge into item 4**: suggested edits are refactorings; until then the hints of text work.
3. **Keep, A**: `sig` and `resolve` are the cheapest answer to 2.3, an agent's view of overloading before it surprises; `refs` after them.
4. **Cut, B**: rename, fill in missing cases, and change a signature with its calls, the three that agents do by error-prone scripts (section 3); `ideas/refactor.md`, 356 lines with edit scripts, replies, replay under concurrent edits, and operations as stored intent, designs far beyond a first version and should shrink to it.
5. **Cut, B**: the suppression of a warning with a reason and the typed appends in loops are needed, since false positives sink rules; the rules of the e-graph (`ideas/lint.md`) are parked.
6. **Keep, A**: follow-ups are what costs an agent most per error, with the two cases of section 3 added.
7. **Park**: agents work through the command line, and `hints` already feeds the editor; the server waits for item 3 and for a person who needs it.

### Testing

1. **Keep, C**: examples as first cases of properties, cheap.
2. **Cut, B**: print `Falsified` readably; abstract types and random values beyond the depth are parked until a lemma needs them.
3. **Cut, B**: the failure message of an example, `gcd(12, 18) gave 3, expected 6`; the parts of `sig` and of the extension follow Diagnostics 3.
4. **Keep, C**: infrastructure of tests, small.
5. **Keep, B**: take the second option, the internal error when the first diagnostic disagrees, which touches no case.
6. **Keep, A**: limits of memory and time per test; an unbounded test once killed a session, and agents cannot see that coming.
7. **Drop as an item**: a crash seen once and never reproduced is no work to do; record it in `BUGS.md` if it recurs.

### Documentation

1. **Keep, A, in progress**: see 2.1.
2. **Merge into item 1**, with the facts of section 3.

### Checker

1. **Cut, C**: the exceptions of the phases as one datatype of `ulang/syntax/error.u` solves the long raises of the helpers of cases; a `try` with a result is a second way to write a handler and stays out unless that does not suffice.
2. **Keep, C**: `decreases` is cheap for the checker, which stays out; else `DESIGN.md` should stop implying that functions terminate.

### Syntax

1. **Cut**: `exists` and `forall` of sets, maps, and arrays stop early by their implementation, without new syntax or a new fold; the bounded quantifiers wait for the specifications that need them.
2. **Keep, C**: errors in the aliases as written.
3. **Keep, B**: lines of 300 characters defeat a canonical layout that is meant to be read.
4. **Park** with the typed checked program, as the item says.
5. **Keep, A**: ascription is the one fix for `UnknownFieldOwner` and ambiguous empties (2.3), and the habit of other languages.
6. **Decide**: `out _` yes, `_` as a formal of a procedure lambda yes, as function lambdas have; a call of an expression and `|` in operators no, each a second way.

### Modules

1. **Merge into Later 1**: making `os` a package is the cheap half of showing effects; decide both together.
2. **Keep, A**: see 2.3; the design in the item is sound, with the qualifier as the last name of the path.
3. **Drop the rediscussion**: a package is its directory, which works; the trap of scratch files is a fact for the documentation (section 3), and a project file for Build 1 needs no new package structure.

### Code

1. **Keep, B, in progress** by `ideas/or-patterns.md`.
2. **Keep, B**: every builtin in six places is the largest cost of extending the runtime, and the HTTP server of `ideas/examples.md` waits on it.
3. **Merge into item 2**.
4. **Keep, C**, with Modules 1.
5. **Drop**: the principles are mostly in Style of `CLAUDE.md` already; add the rest there when an item that needs them is done.

### Library

2. **Keep, A**: `trim`, `lines`, `take_while`, and `drop_while`, which agents reached for twice; not `at` of lists, which invites quadratic loops.

### Build

1. **Cut, A**: `check --test-compiled` first (2.9), with `--list`; `build`, processes, and the Makefile reduced to the seed after it; dependencies parked.
2. **Park** with `ideas/archives.md` until a profile of `--test-compiled` shows that `cc` of the library dominates the loop.

### Compiler

1. **Keep, A**: a runtime error without a position is the worst feedback an agent gets from a run.
2. **Decided** on 2026-11-15: the backend stays and grows with the language, `make testelf` next to `make testc` keeps both backends in step, the object is its path, and `make` switches from C once the assembly build is as fast, see The path of `ideas/assembly.md`.
3. **Park**: no optimization is needed until a program is too slow.
4. **Park**: the memory of the compiler matters only for self-compilation, which takes seconds.

### Verification

Ordered by soundness first, since a prover that proves false is worse than none:

1. **Keep, A**: items 9 and 10, each a case of `ulang/test/errors/lemmas.u` first, as they say; then the small bugs 11, 12, 8, and 13.
2. **Keep, B**: item 7, naming the limit that ended a search, which principle 8 asks; item 5, readable goals; item 6, functions opaque to the default proof, without which proofs do not scale.
3. **Keep, B**: item 1, contracts of functions, the next rung of the ladder.
4. **Park**: items 2, 3, and 4, research beyond what agents meet now.

### Later

1. **Decide with Modules 1**: capabilities, as item 4 describes, fit the language better than a clause `uses os`.
2. **Drop**: record and replay, which capabilities would give for free if taken up.
3. **Keep, C**: native code of users is the gate to real programs, after Build 1.
4. **Keep, C**: the flow check of streams as a lint, as the item concludes.

### Examples

Keep the small programs, which test the language on real tasks and found trip-ups before; the ambitious ones are a wishlist, of which one, `tex.u`, shows a feature, `try` restoring state, that no other does; the HTTP server waits for Code 2.

### The plans

- `ideas/prove.md`, `ideas/or-patterns.md`, and `ideas/assembly.md`: keep, the last with the decision of Compiler 2.
- `ideas/refactor.md`: shrink to the cut of Diagnostics 4.
- `ideas/lint.md`: park; nearly every rule waits for something else.
- `ideas/packages.md`: park until the profile it asks for.
- `ideas/archives.md`: park with Build 2.
- `ideas/review.md`: as of c319a1d, 230 commits ago; check each finding against the sources and drop those that are gone, or delete it.
- `ideas/properties.md`: a research sketch, not open items; keep as the argument behind `DESIGN.md`.

## 5. Order

The ranks `A`, in an order that lets each help the next: Documentation 1; Compiler 1 and Testing 6; Diagnostics 6 and 3; Syntax 5 and Modules 2; Library 2, since done; Build 1 cut to `--test-compiled`; and the soundness items of Verification. Then the decisions without work: Compiler 2, Syntax 6, Modules 1 with Later 1, and the proposals 2.4 to 2.8, each into its item or into `ideas/rejected.md`.

## 6. The review of c079446

Findings of a review of the code of `UnusedImport` and of the alternatives in patterns that are choices of design rather than bugs, each with a proposal; the bugs of that review are fixed.

1. **An error before the first command of its module lands silently on that command.** `with_errors` of `ulang/check/check.u` places an error that no command contains, as of an import, on the first command of its module (`first_of`), so every future error with a wrong position lands there too, where it raised `Unexpected` before and showed the misplacement. The two branches also repeat the same `put`. Proposal: an import is a part of its own with its position, so that its errors have a place, and the fallback goes; else a single branch of `or_else(container(…), first_of(…))` restricted to errors of imports.
2. **The interpreter evaluates the guard of a matched alternative twice.** When a case with alternatives matches but its guard fails, `select` of `ulang/run/run.u` retries all alternatives from the first, so the one that matched is matched again and its guard runs again, which nested alternatives multiply. Proposal: retry only the alternatives after the one that `match_into` took, which needs it to tell which, or select over the cases of `expanded_cases` of `ulang/syntax/syntax.u` from the start, as the backends do.
3. **The backends copy the body of a case per combination of its alternatives.** `expanded_cases` of `ulang/syntax/syntax.u` multiplies nested alternatives out, so `(A | B | C, D | E | F, G | H | I)` is 27 cases, each compiling the whole body and guard into C or assembly, exponential in the positions with alternatives; `ideas/or-patterns.md`, Open, notes it. Proposal: test the alternatives where they stand and jump to one shared body, which binds the variables of each, once a program makes many, and until then a limit with an error.
4. **Unparsed commands are classified by their text.** `code_free_errors` of `ulang/syntax/syntax.u` takes an unparsed command for a lemma or test if its text starts with `lemma ` or `test `, which fails for `test\tfoo()`, a line break after `lemma`, or `test(`, since unparsed files are not formatted, so `compile` refuses a program that it should only warn about. Proposal: `AUnparsed` keeps the kind of command that its first token started, which the parser knows.
