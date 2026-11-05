# Design

## Design notes

What a language would ideally offer for writing code with an AI assistant, which types cheaply but makes mistakes it cannot see: implicit conversions, shadowed names, hidden effects, forgotten cases.

1. **Everything that matters is visible where it happens**: effects and data flow in signatures, no implicit conversions, overloading only across modules and resolved statically by types, and failure in the type, with short propagation like Rust's `?`.
2. **A strict, fast, and precise checker**: inference within functions, types at module boundaries, exhaustive matches, and errors with positions and expected types, since the checker is the feedback loop.
3. **Small and uniform**: few concepts, one way to write each, no context-dependent syntax, and a canonical formatter.
4. **Modules with explicit exports, and records with named fields**, since names are how code is navigated, and clashes and positions are where mistakes happen.
5. **Abstraction over effectful code**, such that patterns like `repsep` are written once.
6. **Specifications in the language**: pre- and postconditions and properties, checked by tests or a prover, since the common mistakes are subtle logic errors rather than type errors.

### Compared to ulang

- **Already there**: explicit data flow with `in`, `out`, and `ref`, pure expressions with effects only in statements, exhaustive matches and inference, a small language without implicit behavior, exceptions as values of a type, named tests with `assert` showing both sides, and procedures as values, which abstract over effectful code such as the parsing of sections in `self/parse.u`.
- **Missing**: positions in the errors of the self-hosted checker and compiler, failure in the types, and contracts, while records exist in part, as named fields with accessors and updates. These are extensions, not redesigns, see `TODO.md`.
- **Questionable**: user-defined notations make the syntax depend on declarations, e.g. whether `and` is an operator or a name, for tools and for writing code alike.
