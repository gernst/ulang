# Design

## Design notes

What a language would ideally offer for writing code with an AI assistant, which types cheaply but makes mistakes it cannot see: implicit conversions, shadowed names, hidden effects, forgotten cases.

1. **Everything that matters is visible where it happens**: effects and data flow in signatures, no implicit conversions, overloading resolved statically and uniquely by types, and failure in the type, with short propagation like Rust's `?`.
2. **A strict, fast, and precise checker**: inference within functions, types at module boundaries, exhaustive matches, and errors with positions and expected types, since the checker is the feedback loop.
3. **Small and uniform**: few concepts, one way to write each, no context-dependent syntax, and a canonical formatter.
4. **Modules with explicit exports, and records with named fields**, since names are how code is navigated, and clashes and positions are where mistakes happen.
5. **Abstraction over effectful code**, such that patterns like `repsep` are written once.
6. **Specifications in the language**: pre- and postconditions and properties, checked by tests or a prover, since the common mistakes are subtle logic errors rather than type errors.

## Principles

1. **Locality.** Every rule should be decidable from the code where it is used, its arguments, and the signatures it refers to, not from other uses in the same command, from which modules happen to be imported, or from line breaks. User-defined notations are the deliberate exception.
2. **One way to write each thing.** Two syntaxes for the same construct are two things to learn, and a chance to mix them up.
3. **Keystrokes are cheap.** A convenience that only saves typing is not worth a rule, since agents don't mind writing. What matters is that the shape of the code is clear.
4. **The cost of a feature** is roughly the length of the paragraph needed to explain it to an agent. In `CLAUDE.md`, the longest paragraphs are about literals and comprehensions, overloading, and the hooks `compare`, `==`, and `show`, which depend on each other, and that is where the complexity is, not in the core.
5. **A small type system, with contracts for the rest.** Types are the decidable part of the specification, with inference inside bodies, no higher-kinded types, and no computation on the level of types, while everything more expressive goes into contracts and lemmas.

## What to keep

These parts are right, and later changes should not weaken them:

- **Pure functions, effectful procedures.** The kind of a declaration says whether its code has effects, which is cheaper than effect annotations and gives most of their value, and with the check of termination, functions terminate.
- **`in`, `out`, and `ref`**, so that data flow shows in signatures and at calls.
- **Atomic effects under exceptions.** Outs and refs reach the caller only on return, and handlers see the variables as they were before the `try`.
- **Procedures as values, and lambdas for both functions and procedures**, which abstract over effectful code, as `repsep` in `self/parse.u`.
- **Static types, with inference inside bodies and types at boundaries.** Types are the one kind of specification that is always checked, complete, decidable, and fast, and they also drive resolution, induction in the prover, abstraction, and compilation.
- **Pattern guards and nested patterns.**
- **Unbounded integers, and deterministic containers**: maps and sets are ordered, so the order of iteration never depends on hashing.
- **Export and implementation sections, with abstract types, and imports that are not transitive**, the basis for signatures later.
- **Expected-error tests by a first-line comment** (`// type error: … at 3.1`), which are cheap to write and exact to check.

## Compared to ulang

- **Already there**: explicit data flow with `in`, `out`, and `ref`, pure expressions with effects only in statements, exhaustive matches and inference, a small language without implicit behavior, exceptions as values of a type, named tests with `assert` showing both sides, procedures as values, which abstract over effectful code such as the parsing of sections in `self/parse.u`, positions in the errors of the parser and the checker, and named fields with accessors and updates.
- **Missing**: positions of runtime errors, failure in the types, termination of functions, and contracts. These are extensions, not redesigns, see `TODO.md`.
- **Questionable**: user-defined notations make the syntax depend on declarations, e.g. whether `and` is an operator or a name, for tools and for writing code alike, but they stay; and the heuristics of overloading, which `TODO.md` replaces by unique resolution.
