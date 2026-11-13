# Lint on the e-graph of the prover

A plan: the rules of `ulang.sh lint` that need the e-graph of the prover, `ulang/prove/egraph.u` (`ideas/prove.md`, section 5), so that each rewrite is an equality that the prover establishes. The walks of the syntax are done in `ulang/lint/lint.u`, described in `CLAUDE.md`, and those still open, with the suppression of warnings, are Diagnostics 5; edits are applied by `refactor fix`, Diagnostics 4 and `ideas/refactor.md`, never by lint. When this work is done, what holds goes into `CLAUDE.md`, what stays open into the items of `ideas/`, and this file is deleted. Its background is the sketch `ideas/properties.md`: structural quality as the cost of verifying a unit and a change to it.

## 1. Why an e-graph

A rewrite is an equality plus a choice of the simpler side. An e-graph separates the two, as `ideas/prove.md` separates provability from the printed goal:

- **Saturation finds the equalities**, over the definitions, the proven lemmas, and the facts of the context, in all directions at once.
- **Extraction chooses** the cheapest representative by a cost, for lint the size of the source.

So lint needs no orientation of rules, no marker of rewrite lemmas, and no matcher of its own: any proven equation of the library may contribute, and the cost decides what is suggested. The patterns of `ulang/lint/lint.u` now, `placed_lints` and `iterated_list`, match by spelling and argue their soundness in docs; on the e-graph a suggestion is in the class of the original only if the prover's rules put it there.

## 2. What enters the e-graph

- **The parts of a body.** Each expression part of a function body, or of a statement of a procedure, is translated by `term_of` of `ulang/prove/translate.u`, with the formals, the binders, and in procedures the locals in scope as fresh constants. The checked program keeps the position of each part, so lint records for each class the parts of the source in it, a side table over the class ids.
- **Definitions** as equations, one rule per leaf and case of the body (`leaf_equations` of `ulang/prove/equations.u`), and the non-recursive ones as abbreviations, also in reverse, a body that spells out `f` gives `f(xs)`, limited:
  - to exported functions, first of the library;
  - to bodies that are a `match`, an `if`, a fold, or a form, so that `a + b` never folds into every function that adds;
  - never inside the definition of `f` itself, by qualified name.
- **Proven lemmas** of the library and of the module's package, e.g. `if c then true else false == c` and `List(for x in xs) == xs` of a list, stated next to their functions and proven when the library is tested, so that a change of a definition fails the proofs of what lint suggests about it.
- **The context.** The e-graph is a value, so the walk extends it at each `if`, guard, and case by the condition or the shape of the case, which gives D1 and D2 from the same saturation.

## 3. How a lint is extracted

Each class has printable representatives, built bottom-up:

- **a source part** of the class, printed as written;
- **a node that a rule or a definition added**, whose source is the right side of that rule as parsed, with each variable printed as the representative of the class it is bound to;
- **nothing else**: nodes of the checker's translations, such as the folds of a form, have no source unless a source part is in their class.

The cost of a representative is the number of tokens of its source. A lint is reported at a part whose class has a printable representative cheaper by a margin, with the edit that replaces the part by it. Then:

- **Kept only if it checks**, by `refactor fix`, which drops it if a name of the replacement resolves to another declaration than in the rule, e.g. in a module that declares its own `value_or`.
- **Strictness.** The prover's equality ignores evaluation: `value_or(o, d)` evaluates `d` where the `match` may not. So a representative from a rule is printable only if each variable that the rule's other side reaches only under a branch, a case, or a lambda is bound to a class with a plain term, a literal, a variable, or a constructor of plain terms; by class, so more precise than `is_plain` of `ulang/lint/lint.u` now.
- **Termination.** Equations of functions that may not terminate can be proven falsely, so only functions whose termination the prover proves (`terminations` of `ulang/prove/termination.u`) contribute, the library first.
- **Budget.** Saturation runs within `Limits` of `ulang/prove/simp.u`, `rounds` and `nodes`, per body.

## 4. Binders

The prover keeps lambdas and matches as opaque leaves, closed terms over de Bruijn indices that are equal only as terms. That splits the rules:

- **Rules whose variables stand for whole leaves** work with opaque leaves: `List(for x in xs) == xs`, whose lambda is closed and fixed; a fold whose function is a variable; the bodies of `value_or` and `not`, matched inside a leaf, which needs shifting the indices of rule variables under the binders of the leaf and entering the bound subterms as classes.
- **Rules that change the inside of a lambda,** such as moving a generator from a copy of a set to the set itself, need rewriting under binders, slotted e-graphs. Until then they are cases that lint misses, not wrong ones.

## 5. What lint needs of `ulang/prove/egraph.u`

Exported now, and enough for the side table: class ids, `class_of`, `class_nodes`, `class_ids`, `add_term`, `lookup_term`, `ematch`, `add_instance`, and the graph as a value. Missing:

- **extraction by a cost given as a parameter**, the term order for goals and the size of the source for lint; extraction exists with a fixed cost, `term_rank` of `ulang/prove/simp.u`, and `smallest` for `printable`, while a cost given as a parameter is missing;
- **the rule and the classes of its variables for each node that a rule added**, for printing;
- **matching inside opaque leaves with shifting**, as above;
- **the equations of non-recursive definitions in both directions**, the reverse one restricted as above.

## 6. The rules

Engines: **eq** (definitions and lemmas), **ctx** (extended by the path), **classes** (parts that share a class). Priority: **A** soon, **B** after the mechanism has settled.

- **R1 `CopiedContainer`** (eq, A for lists, sets and maps under binders): `List(for y in List(for x in c) yield e)` is `List(for y in c yield e)`, also as the generator of a `for`; replaces `iterated_list`.
- **R2 `Folded`** (eq, A): a part that spells out the body of a library function, `value_or`, `map_option`, `not`, `exists`; replaces the `value_or` pattern of `placed_lints`.
- **R4 `RepeatedExpr`** (classes, B, a finding without edit): two source parts in one class, larger than a threshold, in one declaration or module, modulo the names of locals, overloads, and the lemmas, so `x.name` and `name_of(x)` are found too.
- **R5 `BoolIte`** (eq, A): `if c then true else false == c`, `c == true == c`, `if c then a else a == a`; replaces the pattern of `placed_lints`.
- **R6 `IdentityForm`** (eq, A): `List(for x in xs) == xs` of a list, `Set(for x in s) == s` of a set.
- **D1 `DecidedCondition`** (ctx, B): a condition whose class contains `true` or `false` under the path, replaced by the branch it takes.
- **D2 `RedundantGuard`** (ctx, B): `if c then a else b` where `a` and `b` share a class under `c`, which becomes `b`.
- **N1 `DuplicateDefinition`** (classes, B): two functions of the same type whose bodies, applied to the same fresh constants, share a class after saturation.
- **P1 `ulang.bin diff <old> <new>`** (classes, B): the declarations added, removed, or changed, where a change is a body that no longer shares a class with the old one, so a renamed local or an applied lint is no change; also the base of the merge by declarations of `refactor.md`.

Rules that wait for other items, kept here until those take them:

- **F3 `MaskedDomain`** (ctx, Verification 1): an `if` whose condition is the negation of the `requires` of a call in the other branch, with a constant in its own, e.g. `if d == 0 then 0 else n / d`.
- **H1 `HookAgreement`** (eq, Testing 1): a declared `compare` must agree with structural equality, as a generated lemma, else tried on generated values.
- **N2 `DivergentDuplicate`** (Testing 1): functions of the same type and similar shape whose bodies do not share a class, with the generated values where they differ.
- **T4 `UnusedExport`** (opt-in, `refs` of Diagnostics 3); **T5 `ulang.bin coverage`**, the declarations that no test or example reaches, by `callees` and `reachable` of `ulang/prove/translate.u`; **L1 `ulang.bin deps file:name`**, the definitions whose bodies a proof would unfold.
- **G1** a permutation of the arguments that fits a candidate, and **G2** the nearest visible names and the packages not imported that export the name, as hints of Diagnostics 2.

## 7. Order

1. **Into `ideas/prove.md`:** the needs of section 5, which only this file lists, so that the session of the prover takes them before extraction is designed.
2. **Lint on the e-graph:** the side table of source parts per class and extraction by the size of the source; R2 by definitions; the lemmas of R5, R6, and R1 of lists, which replace the patterns of `placed_lints` and `iterated_list`. What stays: the command, `Warning`, `format_module_expr` for the representatives, the walks of statements, and the edits for `refactor fix`.
3. **The context:** D1, D2.
4. **Classes:** R4, N1, P1.
5. **Rewriting under binders**, if the prover takes slotted e-graphs: R1 of sets and maps, and generators in general.
6. **Back to checked expressions:** `expr_of(Term)`, and extraction by the cost of evaluation for the optimizations of Compiler 3.
7. **After their items:** F3, H1, N2, T4, T5, L1, G1, G2.

When done, the items change: Diagnostics 5 keeps only what is open; item 2 gains G1 and G2; Testing 1 names N2 and H1; Compiler 3 takes the extraction by the cost of evaluation; Code 1 and Code 6 are checked by rules rather than listed by hand; `CLAUDE.md` says which rules of Style `lint` checks, and that the lemmas of the library are also what lint suggests.

## 8. Open questions

1. **Which lemmas enter the e-graph of lint:** all proven equations of the library and of the module's package, or only those of the library? Extraction only suggests what is cheaper, so all of them are probably fine.
2. **Which definitions are used in reverse:** the library's first; should a project's own exported functions fold in the modules that import them?
3. **Where the table of source parts lives:** in lint, over the class ids, or as an analysis per class in the e-graph.
4. **The cost of saturation per body**, unmeasured: measure on `ulang/ulang.u` against `check`, and set the budget from that.
5. **Should the default set of `lint` run in `make test`** once the repository is clean, and **should `UnusedPrivate` be an error**, as unused imports will be?
