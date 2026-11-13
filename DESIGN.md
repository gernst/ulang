# Design

## Premise

ulang is written by agents, which type for nothing but make mistakes they cannot see: implicit conversions, shadowed names, hidden effects, forgotten cases, swapped arguments, and handlers that mask a failure. So the measure of the design is the cost to verify, how much a reader, a person, a model, or a tool, must know and check to trust a unit and a change to it, and the checker is the feedback loop through which an agent learns the language, which is in none of its training data. The properties that this cost breaks into, and how far ulang gives each, are `ideas/properties.md`.

## Principles

1. **Locality.** Every rule is decidable from the code where it is used, its arguments, and the signatures it refers to: not from other uses in the same command, from preferences among candidates, or from line breaks. Overloading resolves uniquely by types or fails with the candidates, and imports are not transitive. The exceptions are deliberate: user-defined notations, which make parsing depend on the imports, overloading across modules, which makes resolution depend on them, and the hooks `compare`, `==`, and `show`, which make a builtin depend on a function of the module of the type.
2. **Comments are syntax.** Prose is a doc on a module, a command, a constructor, a statement, or a `case`, kept in the tree and printed back by the formatter, so that no tool loses or moves a comment; a note at the end of a line or detached in a body is a parse error, since the code there needs a name. A doc that makes a claim about values is better an example, which runs.
3. **One way to write each thing.** Two syntaxes for one construct are two things to learn and a chance to mix them up: `List(1, 2)`, never `[1, 2]`; a nested `if`, never `elif`. What was rejected is recorded with its reasons in `ideas/rejected.md`, so that no later session adds it back.
4. **Keystrokes are cheap; tokens read are not.** A convenience that only saves typing is not worth a rule: every statement starts with its keyword, and every argument of a call carries its mode. Such verbosity is low in entropy, read at a glance; what costs a reader is semantic redundancy, a doc that restates a name, which the rules of style forbid.
5. **The cost of a feature** is roughly the length of the paragraph that explains it to an agent. In `CLAUDE.md` the longest paragraphs of the language are those of positions and errors, of examples, of the forms, of contracts and claims, of the hooks, and of raises, so that is where the complexity is, not in the core.
6. **A small type system, with contracts for the rest.** Types are the decidable part of the specification, always checked: inference inside bodies, types at boundaries, no higher kinds, no computation on types. What is more expressive goes up a ladder of evidence: examples on signatures, lemmas tested on enumerated values, lemmas proven, and contracts of procedures proven by their claims.
7. **Effects show in signatures and at calls.** The kind of a declaration is a one-bit effect system, functions pure, procedures not; data flow shows in the modes; what a procedure raises is part of its type, inferred for private procedures and declared at exports, so the annotations stand where readers cross a boundary.
8. **Failure is never silent.** Functions do not raise and fail only by the exceptions of builtins, which are defects; a failed `assert` cannot be caught; matches are exhaustive; lookups give options, with a default named at the use; generators do not skip elements whose shape does not match; and a handler of what its body cannot raise is an error.
9. **Determinism.** Integers are unbounded; maps and sets are ordered by one total order of all values, so iteration never depends on hashing; the interpreter and both backends agree, with the texts of shared runtime errors in one place; the prover works within named limits; and the tests of lemmas enumerate the smallest values instead of drawing random ones, so every result an agent sees is reproducible.
10. **Every departure from a habit has a hint.** Agents arrive with the habits of other languages, and ulang rejects some on purpose; the error at the place of the habit names what to write instead, after `; ` of its message. A departure that cannot be given such a hint is a sign that it is arbitrary.
11. **The layout is canonical, and the checker enforces it.** `format` writes the only accepted layout, and `check`, `lint`, and `test` reject any other, so no change reformats code it does not touch, and every file that checks has passed a round trip through parser and printer.

Two consequences shape the rest. **The parser desugars nothing**: forms, `for`, lambdas with patterns, fields, `let` of patterns, and aliases stay as written until the checker translates them, so the formatter prints what the source wrote and errors are reported against it. And **the tools are part of the language**: formatting, lints, refactorings, and the prover read and write the same tree, and the commands of one binary are the interface through which an agent works.

## What to keep

These parts are right, and later changes should not weaken them:

- **Pure functions, effectful procedures**, which give most of the value of effect annotations at the price of one keyword.
- **`in`, `out`, and `ref`**, so that data flow shows in signatures and at calls.
- **Atomic effects under exceptions.** Outs and refs reach the caller only on return, and handlers see the variables as they were before the `try`, so reviewing a handler never asks which assignments happened before the exception; and backtracking needs no undo code.
- **Checked exceptions**, inferred where they are private and declared where they are exported, with unreachable handlers as errors.
- **Procedures as values, and lambdas for both functions and procedures**, which abstract over effectful code, as `parse_separated` of `ulang/parse/parse.u`.
- **Static types with inference inside bodies and types at boundaries**, which also drive resolution, induction in the prover, abstraction, and compilation.
- **Overloading without preferences**: a use compiles only if exactly one candidate fits, and otherwise the error lists the candidates, so no reader simulates a rule of specificity.
- **Pattern guards, nested patterns, and pattern aliases**, which expand to constructors, so exhaustiveness and the backends see only those.
- **Unbounded integers, and deterministic containers.**
- **Packages with export and implementation sections, abstract types, and imports that are not transitive**, where a directory is a package and its members see each other.
- **Specifications in the source**: examples on signatures, which document a call and run as tests; lemmas, tested on enumerated values before they are proven; and contracts with their claims.
- **Errors as part of the program**: every part has a position, a type error stands at the innermost part around it, the parser and the checker continue past errors and report all of them in one run, as JSON lines, where a part that does not parse or does not check has the type `TError`, which makes no follow-ups, and a program with errors still runs as far as they allow.

And in the implementation:

- **Phases as packages whose imports check the layers**: `ulang/syntax`, `ulang/format`, `ulang/parse`, `ulang/check`, `ulang/run`, `ulang/compile`, `ulang/prove`, and `ulang/lint`, each importing only those before it, and the driver `ulang/ulang.u` all of them.
- **Errors as data**: diagnostics are values with positions, recorded by `report` of `ulang/check/check.u` while the check continues, and kept in the checked program as error nodes `AErr`, which each later phase treats as a part that does not parse; each command starts with a fresh substitution and numbering of unknowns (`enter`), and is solved and zonked at its end.
- **Narrow interfaces**: the functions `with_*`, `*_of`, and `is_*` of `Sig` keep the layout of its maps private.
- **Tests across components**: the printer by its round trip through the parser, which every file that checks passes; the interpreter against compiled code (`make testc`); the errors that a phase must raise as cases of `ulang/test/errors/`, each expecting the exception and its value from a run; and the references of the documentation by `checks/docs.u`.
- **Bootstrap**: the seed is the C of the last compiler that compiled itself to the same C, so a change that breaks the compiler cannot replace it.
- **Backends**: C as a small typed syntax with a printer that adds parentheses (`ulang/compile/c.u`), and x86-64 assembly by instructions as data (`ulang/compile/asm.u`), not built from strings, with the analyses that both share in `ulang/compile/lower.u`; one type descriptor per datatype with `compare`, `equals`, and `show`, mirrored by the maps of `Defs`; outs and refs copied in and out alike in all backends; and fused comprehensions that fall back to the general fold.
- **The prover**: terms with de Bruijn indices for binders and levels for the variables of open sequents, with one `rebind` and its invariant written down; an e-graph with congruence closure; canonical sums with Fourier–Motzkin elimination and integer tightening; named limits (`Limits` of `ulang/prove/simp.u`); recursive functions unfolded only once their termination is proven; and end-to-end tests that include the goals expected to stay open.

## Tensions

Where the design is still in tension with its premises, each with the item that would resolve it:

- **New syntax against no training data.** The bet is that explicit, uniform syntax with hints costs agents less than familiar but ambiguous syntax; the trip-ups of `unclear.md` and of agents new to ulang are anecdotes, not a measurement, and the follow-ups that the errors still make are Diagnostics 8.
- **The hooks are the weakest point of locality**, since a declared `compare` must agree with structural equality, which nothing checks (`H1 HookAgreement` of `ideas/lint.md`).
- **The order of arguments is the remaining hazard of grounding**: named fields protect constructors, types most calls, but not `slice(s, i, j)`; examples on signatures are the answer, with a hint that tries permutations (`G1` of `ideas/lint.md`).
- **Exceptions cannot become values**, so code that collects errors catches each kind into an out (Checker 1), and there is no polymorphism over raised sets (Checker 4).
- **Termination is proven only for the recursive functions that lemmas reach**, so the split of functions and procedures does not yet guarantee that functions terminate (Checker 2).
- **The library is ambient**, also `os`, so its procedures of the system show in no signature (Modules 1, Later 1).
- **Verbosity has a cost in context after all**: `CLAUDE.md` describes the language, the implementation, and the workflow together in some 12,000 words loaded into every session, which a reference of the language for writers of programs would split (Documentation 1).
- **Limits make proofs deterministic, not robust**: a proof that closes at one limit may open at a smaller one, and the default proof does not say which limit ended its search (Verification 7).
