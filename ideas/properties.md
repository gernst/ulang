# Properties of code that is cheap to check

A research sketch of the structural properties that code written by agents should have, and of how far ulang gives them by construction. Each property names what ulang already does and the items of the other files of `ideas/` that would give the rest, so that this file holds the argument and those files the work.

## 1. Thesis

Writing code is now nearly free, and checking, reviewing, and changing it is where the cost is. So structural quality is measured by the **cost to verify**: how much a verifier, a person, a model, or a tool, must know and check to trust a unit and a change to it. Three assumptions behind the classic norms no longer hold:

- **The writer has no stake in the code**: it will not maintain it and does not remember last week's conventions.
- **The writer produces much, and its mistakes look plausible**: fewer slips of syntax, more well-formed code that misreads the intent.
- **The reader is often a machine**: another agent, a checker, a prover, or a reviewing model.

This is the premise of the design notes of `DESIGN.md`, whose principle 4, that keystrokes are cheap, is the same observation from the side of the language. Some norms get stronger under it, locality and explicitness; some weaker, the abstraction that saves typing; and some are new, failing loudly and keeping diffs proportional.

## 2. Properties

Each property has a definition, a formal handle, what ulang does, and what is open.

### 2.1 Locality

**Definition.** A unit is understood and verified from a small, explicitly identified set of facts: its text, the contracts of what it calls, and its declared inputs and effects.

**Formal handle.** The verification context of a unit, the smallest set of definitions, contracts, and invariants that a modular verifier needs for its obligations, is small and declared, as frame conditions are in separation logic and dynamic frames. A proxy is the number of non-local symbols whose meaning, not only their type, a proof depends on.

**In ulang.** Principle 1 of `DESIGN.md`. Functions are pure and see no state; procedures see only their formals, and their effects reach the caller as their outs and refs, atomically on return; there are no globals but functions without parameters, which are constants; overloading resolves by the types at the use alone, never by preferences; and the default proof of a lemma unfolds only what it reaches, which `L1 ulang.bin deps` of `ideas/lint.md` would list.

**Open.** Procedures may use all of `os` invisibly (Later 1, `uses os`); user-defined notations depend on the imports, a deliberate exception (`rejected.md`); and contracts of functions, which would make the context of a call its contract instead of its body, are not implemented (Verification 1), while lemmas about functions that are not recursive are never used, since such functions always unfold (Verification 6).

### 2.2 Explicitness

**Definition.** Behaviour is determined by visible text: no hidden conventions, implicit globals, magic defaults, reflection, or registration that changes meaning without appearing at the use.

**Formal handle.** A non-interference property: the observable behaviour of a unit is a function of its declared inputs, declared effects, and text alone. Locality is about the size of that context, explicitness about whether it is declared.

**In ulang.** Every statement starts with its keyword, `call`, `assign`, `raise`; the modes show at every call, `call p(in a, out b, ref c)`; what a procedure raises is checked and, if exported, declared (`UndeclaredRaise`); no formal or local hides another (`HiddenLocal`); and there are no implicit conversions. The hooks `show`, `compare`, and `==` are the one convention that changes the meaning of a builtin from elsewhere, bounded by their fixed shape (`MisshapenHook`) and by their module being that of the datatype.

**Open.** The effects of `os` (Later 1); the polymorphism over raised sets, without which a generic `retry` must fix what its argument raises (Checker 4); and the hooks of `compare`, which must agree with structural equality unchecked (`H1 HookAgreement` of `ideas/lint.md`).

### 2.3 Non-redundancy

**Definition.** Knowledge that must stay consistent has one source. The hazard is **divergent duplication**, copies that are meant to agree and can drift apart; code that looks alike but encodes independent decisions should stay apart, since merging it couples what should evolve separately.

**Formal handle.** Two fragments are redundant if they are equivalent, or meant to be, up to parameters, and divergently redundant if no shared definition ties them. This separates harmful clones from coincidental similarity, which syntactic clone detectors cannot. It pulls against locality: a shared definition adds a dependency to the context of each user.

**In ulang.** One concept has one name (Style of `CLAUDE.md`), the library shares names and orders of arguments across its containers, and the e-graph of the prover can decide equivalence where syntax cannot: `N1 DuplicateDefinition` and `R4 RepeatedExpr` of `ideas/lint.md` find equivalent bodies modulo names and lemmas.

**Open.** Those rules wait for the e-graph lints; `N2 DivergentDuplicate`, functions of one type and shape that differ on generated values, waits for tests with formals (Testing 1); and the repository's own facts kept in several places are Code 2.

### 2.4 Proportionality

**Definition.** A change is as large and as spread as the change in intent: no drive-by refactors, speculative generality, unrequested features, or reformatting of untouched code.

**Formal handle.** Every hunk of a diff is necessary: reverting it alone breaks the new specification or an existing test. That is checkable per hunk.

**In ulang.** The canonical format, which `check` enforces (`require_formatted` of `ulang/format/format.u`), makes reformatting impossible as a side effect; refactorings are operations with declared effects, refused if any use other than those intended resolves differently (Diagnostics 4, `refactor.md`); and `P1 ulang.bin diff` of `ideas/lint.md` would show a change by declarations, where a renamed local is no change.

**Open.** The necessity of hunks is not checked; it needs `ulang.bin diff`, the tests that each hunk breaks, and a definition of what a hunk is in a tree, a declaration or a part.

### 2.5 Failing loudly

**Definition.** A violated precondition, invariant, or expectation surfaces instead of being absorbed. Models prefer fallbacks, catch-all handlers, and defaults that hide bugs, e.g. a lookup whose missing key gives `0`.

**Formal handle.** On every path where a callee's precondition or an invariant is violated, the code ends in an observable failure, an exception, an error passed to the caller, or an abort, never a normal-looking result. As refinement: the implementation may not be more defined than the specification where the specification is undefined, the reverse of the usual rule. This property is underexplored and specific to generated code.

**In ulang.** Builtins raise instead of guessing: `at` and the strict `slice` raise `OutOfBounds`, `chr` raises `NoCharacter`, accessors raise `MissingField`; lookups give an `Option`, so a default is visible at the use, `value_or(get(k, m), 0)`; matches are exhaustive; handlers of what nothing raises are errors (`NeverRaised`); a failed `assert` cannot be caught; and a handler that only skips is the lint `SwallowedException` (`swallowed` of `ulang/lint/lint.u`).

**Open.** A handler `case _ =>` that assigns a default masks as well as one that skips, which no rule finds yet; `F3 MaskedDomain` of `ideas/lint.md`, a branch that returns a constant where a `requires` would fail, waits for Verification 1, which also gives the reversed refinement its specification; and an entry may let a checked exception escape (Checker 5).

### 2.6 Grounding

**Definition.** Every referenced name, argument order, bound, and default exists and means what the code assumes: not the crude case of a function that does not exist, which fails fast, but a swapped order of arguments, an inclusive bound taken as exclusive, or a misremembered default.

**Formal handle.** Each use of an external entity carries an assumption about it, and grounding means that its contract implies every such assumption, so that with machine-readable contracts grounding becomes contract checking.

**In ulang.** Unknown names are errors, overloading picks only declarations whose types fit, and the examples on signatures state what the order of arguments and the edges mean, `example slice("abcd", 1, 3) == "bc"`, as tests that cannot go stale (Style of `CLAUDE.md`).

**Open.** Contracts of the library, `requires d != 0` of `/` (Verification 1); `G1` of `ideas/lint.md`, a permutation of the arguments that fits a candidate, as a hint; and examples visible where the call is written, in `sig` and the hover (Testing 3).

### 2.7 Traceability

**Definition.** Every piece of code traces to a requirement, a clause of a specification, or a decision: no orphan code that exists because the writer thought it useful.

**Formal handle.** A relation from code to specification that is total on the side of the code, the reverse of coverage, which asks that every requirement has code.

**In ulang.** Private declarations that no export, test, lemma, example, or entry reaches are `UnusedPrivate` (`unused_privates` of `ulang/lint/lint.u`), the reachability half of it.

**Open.** `T4 UnusedExport` and `T5 ulang.bin coverage` of `ideas/lint.md`. Links from code to requirements, provenance recorded by the writer, have no design and may not be worth one: in ulang, the nearest thing to a requirement is a test, an example, or a lemma, so traceability may reduce to every declaration being reached by one of them.

### 2.8 Uniformity

**Definition.** Each recurring concern, errors, retries, logging, configuration, is handled by one idiom in a code base. Models draw idioms from their whole training data, and each variant must be checked again.

**Formal handle.** The number of distinct semantic patterns per concern, or, building on 2.3, similar but not identical solutions to one concern as divergent redundancy at the level of idioms. Probably a metric more than a semantic property.

**In ulang.** Principle 3 of `DESIGN.md`, one way to write each thing, with the decisions of `rejected.md`: one loop over containers, no early exit, `if` that always nests, containers by forms, failure by checked exceptions only, and one layout by `format`. Folds that stop early are deferred since each is another way to write a loop (Syntax 1).

**Open.** Idioms above the language, such as how a package reports its errors, have no check; the `R2 Folded` rule of `ideas/lint.md`, code that spells out a library function, is the start of one.

### 2.9 Proportionate defensiveness

**Definition.** Runtime checks are neither missing where contracts are weak nor redundant where types or contracts already guarantee the property. Each redundant check hides the real contract, and often becomes masking (2.5).

**Formal handle.** A check is redundant if a verifier proves its condition from the types and contracts in scope, and missing if a callee's precondition is not implied at the call: both decidable relative to a chosen verifier.

**In ulang.** There is no null, so a whole class of checks has nothing to check, and an exhaustive match makes the cases of a datatype explicit instead of tested; `D1 DecidedCondition` and `D2 RedundantGuard` of `ideas/lint.md` are the redundant half, under the facts of the path.

**Open.** The missing half needs the `requires` of functions (Verification 1).

## 3. Levers of the language

The levers that a language or toolchain has for these properties, and where ulang stands:

| Lever | Properties | ulang |
|---|---|---|
| Effects in signatures | locality, explicitness | pure functions, modes, checked raises; `uses os` and raised sets open (Later 1, Checker 4) |
| Contracts as part of signatures | locality, failing loudly, grounding | contracts and claims of procedures; of functions open (Verification 1, `prove.md`) |
| Canonical forms | uniformity, proportionality | one construct each, the format enforced by `check` |
| Equivalence instead of identity of definitions | non-redundancy | the e-graph lints (`ideas/lint.md`), where Unison identifies definitions by the hash of their syntax |
| Provenance of code | traceability | reachability by tests, examples, and lemmas only |
| No implicit fallbacks in the library | failing loudly | builtins raise, lookups give options, defaults are named at the use |
| Edits as typed transformations | proportionality | refactorings (Diagnostics 4, `refactor.md`) |

## 4. Research questions

ulang is a testbed for them: small enough to change, with its own checker, prover, and lints, and agents as its main writers.

1. **Formalization.** Which properties have semantic definitions, and which only proxies? Hypothesis: locality, explicitness, divergent redundancy, failing loudly, and proportionate defensiveness can be defined relative to a verifier, here the checker and the prover; uniformity and proportionality stay partly metrics.
2. **Empirics.** How does the code of models differ from code by people on these properties, e.g. more defensiveness and masking, less reuse? The findings of agents new to ulang, now items of `diagnostics.md`, `syntax.md`, and `documentation.md`, are one source.
3. **Effect on cost.** Do the properties predict the cost downstream, the effort of verifying, defects, the success of later edits by agents? A design: the same function in structurally different variants, and how reliably a later agent or the prover changes each correctly.
4. **The reader.** Do the properties that help people review also help models and verifiers? Hypothesis: explicitness matters more for models, and indirection hurts them more than verbosity, so some advice in favour of abstraction flips.
5. **Trade-offs.** Non-redundancy pulls against locality, explicitness against brevity. Is there one cost model, the effort to verify, under which the optimum can be stated and computed?
6. **Enforcement.** Can the properties be checks inside the loop of generation, or signals of training, rather than analyses afterwards? In ulang the checker and the lints already run in the loop of the agent; what is the cheapest enforcement that pays?
7. **Design.** What does a language look like whose types guarantee a bounded context of verification, and what does it give up? ulang's answer so far is the split of functions and procedures with modes and checked raises; its limits are the items above.
