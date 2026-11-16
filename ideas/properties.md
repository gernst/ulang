# Properties of code that is cheap to check

A research sketch of the structural properties that code written by agents should have, what of each is open in ulang, and the items of the other files of `ideas/` that would give it, so that this file holds the argument and those files the work. What ulang already gives is in the principles of `DESIGN.md`.

## 1. Thesis

Writing code is now nearly free, and checking, reviewing, and changing it is where the cost is. So structural quality is measured by the **cost to verify**: how much a verifier, a person, a model, or a tool, must know and check to trust a unit and a change to it. Three assumptions behind the classic norms no longer hold: the writer has no stake in the code and does not remember last week's conventions; it produces much, and its mistakes look plausible, well-formed code that misreads the intent; and the reader is often a machine. Some norms get stronger under it, locality and explicitness; some weaker, the abstraction that saves typing; and some are new, failing loudly and keeping diffs proportional.

## 2. Properties

Each property has a definition, a formal handle, and what is open.

### 2.1 Locality

**Definition.** A unit is understood and verified from a small, explicitly identified set of facts: its text, the contracts of what it calls, and its declared inputs and effects (principle 1 of `DESIGN.md`).

**Formal handle.** The verification context of a unit, the smallest set of definitions, contracts, and invariants that a modular verifier needs for its obligations, is small and declared, as frame conditions are in separation logic and dynamic frames. A proxy is the number of non-local symbols whose meaning, not only their type, a proof depends on, which `L1 ulang.bin deps` of `ideas/lint.md` would list.

**Open.** Procedures may use all of `os` invisibly (Later 1); contracts of functions, which would make the context of a call its contract instead of its body, are missing (Verification 1); and lemmas about functions that are not recursive are never used, since such functions always unfold (Verification 6).

### 2.2 Explicitness

**Definition.** Behaviour is determined by visible text: no hidden conventions, implicit globals, magic defaults, reflection, or registration that changes meaning without appearing at the use.

**Formal handle.** A non-interference property: the observable behaviour of a unit is a function of its declared inputs, declared effects, and text alone. Locality is about the size of that context, explicitness about whether it is declared.

**Open.** The effects of `os` (Later 1), and the hooks `compare` and `==`, the one convention that changes the meaning of a builtin from elsewhere, where a declared `compare` must agree with structural equality unchecked (`H1 HookAgreement` of `ideas/lint.md`).

### 2.3 Non-redundancy

**Definition.** Knowledge that must stay consistent has one source. The hazard is **divergent duplication**, copies that are meant to agree and can drift apart; code that looks alike but encodes independent decisions should stay apart, since merging it couples what should evolve separately.

**Formal handle.** Two fragments are redundant if they are equivalent, or meant to be, up to parameters, and divergently redundant if no shared definition ties them. This separates harmful clones from coincidental similarity, which syntactic clone detectors cannot. It pulls against locality: a shared definition adds a dependency to the context of each user.

**Open.** The lints `RepeatedExpr` and `DuplicateDefinition` find repeats modulo names only; equivalence modulo lemmas is `R4` and `N1` on the e-graph of `ideas/lint.md`; `N2 DivergentDuplicate`, functions of one type and shape that differ on generated values, can use the tests with formals (`property_stmt` of `ulang/test/generate.u`); and the repository's own facts kept in several places are Code 2.

### 2.4 Proportionality

**Definition.** A change is as large and as spread as the change in intent: no drive-by refactors, speculative generality, unrequested features, or reformatting of untouched code.

**Formal handle.** Every hunk of a diff is necessary: reverting it alone breaks the new specification or an existing test. That is checkable per hunk.

**Open.** The canonical format and the refactorings of `ulang.sh refactor` keep diffs small, but the necessity of hunks is not checked; it needs `P1 ulang.bin diff` of `ideas/lint.md`, which shows a change by declarations, where a renamed local is no change, the tests that each hunk breaks, and a definition of what a hunk is in a tree, a declaration or a part.

### 2.5 Failing loudly

**Definition.** A violated precondition, invariant, or expectation surfaces instead of being absorbed. Models prefer fallbacks, catch-all handlers, and defaults that hide bugs, e.g. a lookup whose missing key gives `0` (principle 8 of `DESIGN.md`).

**Formal handle.** On every path where a callee's precondition or an invariant is violated, the code ends in an observable failure, never a normal-looking result. As refinement: the implementation may not be more defined than the specification where the specification is undefined, the reverse of the usual rule. This property is underexplored and specific to generated code.

**Open.** A handler `case _ =>` that assigns a default masks as well as one that skips, which `SwallowedException` finds, but no rule finds the former; `F3 MaskedDomain` of `ideas/lint.md`, a branch that returns a constant where a `requires` would fail, waits for Verification 1, which also gives the reversed refinement its specification.

### 2.6 Grounding

**Definition.** Every referenced name, argument order, bound, and default exists and means what the code assumes: not the crude case of a function that does not exist, which fails fast, but a swapped order of arguments, an inclusive bound taken as exclusive, or a misremembered default.

**Formal handle.** Each use of an external entity carries an assumption about it, and grounding means that its contract implies every such assumption, so that with machine-readable contracts grounding becomes contract checking.

**Open.** Contracts of the library, `requires d != 0` of `/` (Verification 1); `G1` of `ideas/lint.md`, a permutation of the arguments that fits a candidate, as a hint; and the examples on signatures, which state the order of arguments and the edges, visible where the call is written, in the hover (Testing 3).

### 2.7 Traceability

**Definition.** Every piece of code traces to a requirement, a clause of a specification, or a decision: no orphan code that exists because the writer thought it useful.

**Formal handle.** A relation from code to specification that is total on the side of the code, the reverse of coverage, which asks that every requirement has code.

**Open.** `UnusedPrivate` gives the reachability of private declarations; `T4 UnusedExport` and `T5 ulang.bin coverage` of `ideas/lint.md` the rest. Links from code to requirements have no design and may not be worth one: in ulang, the nearest thing to a requirement is a test, an example, or a lemma, so traceability may reduce to every declaration being reached by one of them.

### 2.8 Uniformity

**Definition.** Each recurring concern, errors, retries, logging, configuration, is handled by one idiom in a code base. Models draw idioms from their whole training data, and each variant must be checked again (principle 3 of `DESIGN.md`).

**Formal handle.** The number of distinct semantic patterns per concern, or, building on 2.3, similar but not identical solutions to one concern as divergent redundancy at the level of idioms. Probably a metric more than a semantic property.

**Open.** Idioms above the language, such as how a package reports its errors, have no check; `R2 Reimplemented` of `ideas/lint.md`, code that spells out a library function, is the start of one.

### 2.9 Proportionate defensiveness

**Definition.** Runtime checks are neither missing where contracts are weak nor redundant where types or contracts already guarantee the property. Each redundant check hides the real contract, and often becomes masking (2.5).

**Formal handle.** A check is redundant if a verifier proves its condition from the types and contracts in scope, and missing if a callee's precondition is not implied at the call: both decidable relative to a chosen verifier.

**Open.** The redundant half under the facts of the path is `D1 DecidedCondition` and `D2 RedundantGuard` on the e-graph of `ideas/lint.md`, beyond the conditions written alike that `DecidedCondition` finds now; the missing half needs the `requires` of functions (Verification 1).

## 3. Levers of the language

The levers that a language or toolchain has for these properties, and what of each is open in ulang:

| Lever | Properties | Open in ulang |
|---|---|---|
| Effects in signatures | locality, explicitness | `uses os` (Later 1) |
| Contracts as part of signatures | locality, failing loudly, grounding | contracts of functions (Verification 1) |
| Equivalence instead of identity of definitions | non-redundancy | the e-graph lints (`ideas/lint.md`), where Unison identifies definitions by the hash of their syntax |
| Provenance of code | traceability | beyond reachability by tests, examples, and lemmas |
| Edits as typed transformations | proportionality | the refactorings of Diagnostics 4 and `ideas/refactor.md` |

## 4. Research questions

ulang is a testbed for them: small enough to change, with its own checker, prover, and lints, and agents as its main writers.

1. **Formalization.** Which properties have semantic definitions, and which only proxies? Hypothesis: locality, explicitness, divergent redundancy, failing loudly, and proportionate defensiveness can be defined relative to a verifier, here the checker and the prover; uniformity and proportionality stay partly metrics.
2. **Empirics.** How does the code of models differ from code by people on these properties, e.g. more defensiveness and masking, less reuse? The findings of agents new to ulang, now items of `diagnostics.md`, `checker.md`, `syntax.md`, and `documentation.md`, are one source.
3. **Effect on cost.** Do the properties predict the cost downstream, the effort of verifying, defects, the success of later edits by agents? A design: the same function in structurally different variants, and how reliably a later agent or the prover changes each correctly.
4. **The reader.** Do the properties that help people review also help models and verifiers? Hypothesis: explicitness matters more for models, and indirection hurts them more than verbosity, so some advice in favour of abstraction flips.
5. **Trade-offs.** Non-redundancy pulls against locality, explicitness against brevity. Is there one cost model, the effort to verify, under which the optimum can be stated and computed?
6. **Enforcement.** Can the properties be checks inside the loop of generation, or signals of training, rather than analyses afterwards? In ulang the checker and the lints already run in the loop of the agent; what is the cheapest enforcement that pays?
7. **Design.** What does a language look like whose types guarantee a bounded context of verification, and what does it give up? ulang's answer so far is the split of functions and procedures with modes and checked raises; its limits are the items above.
