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
2. **Comments are syntax.** Prose is a doc on a module, a command, a constructor, or a `case`, kept in the tree and printed back by the formatter, so that no tool loses or moves a comment; a note in the middle of a body is a sign that the code needs a name. Free-form and trailing comments are not part of the language, and are discarded.
3. **One way to write each thing.** Two syntaxes for the same construct are two things to learn, and a chance to mix them up.
4. **Keystrokes are cheap.** A convenience that only saves typing is not worth a rule, since agents don't mind writing. What matters is that the shape of the code is clear.
5. **The cost of a feature** is roughly the length of the paragraph needed to explain it to an agent. In `CLAUDE.md`, the longest paragraphs are about literals and comprehensions, overloading, and the hooks `compare`, `==`, and `show`, which depend on each other, and that is where the complexity is, not in the core.
6. **A small type system, with contracts for the rest.** Types are the decidable part of the specification, with inference inside bodies, no higher-kinded types, and no computation on the level of types, while everything more expressive goes into contracts and lemmas.

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

And in the implementation:

- **Errors as data**: type errors are values, with positions added once per declaration by `Located`, and each command starts with a fresh substitution and numbering of unknowns, and is resolved and zonked at its end.
- **Narrow interfaces**: the functions `with_*`, `*_of`, and `is_*` of `Sig` keep the layout of its maps private.
- **Tests across components**: the printer by its round trip through the parser (`make testformat`), which keeps the precedences of both honest, and the interpreter against compiled code (`make testc`), with the texts of shared runtime errors in one place, `self/error.u`.
- **Backends**: C as a small typed syntax with a printer that adds parentheses (`self/c.u`), not built from strings; one type descriptor per datatype with `compare`, `equals`, and `show`, mirrored by the maps of `Defs`; outs and refs copied in and out alike in both backends; and fused comprehensions that fall back to the general fold.
- **The prover**: locally nameless terms with a single `rebind` and its invariant written down, splits kept only if every case closes, canonical sums with Fourier–Motzkin elimination and integer rounding, and end-to-end tests that include the goals expected to stay open.
