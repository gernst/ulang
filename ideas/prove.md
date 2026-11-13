# Verification

Lemmas about functions with proofs, the prover in `ulang/prove/`, contracts of procedures, and claims `correct p` that a procedure meets its contract, proven by invariants, are implemented; contracts of functions are a design. This note has both: what holds now is marked as implemented, the rest is the plan. It merges the former `prover.md`, the plan of the prover's rebuild, into the design of verification.

## Contents

1. Status
2. Specifications (design)
3. Meaning, obligations, loops, and points of the code (design)
4. The proof language (implemented, version 1)
5. Simplification (implemented)
6. Definitions, rewriting, and termination (implemented)
7. Decision procedures
8. Next steps
9. Proposals
10. Open points

## 1. Status

Implemented, on `main`:

- **Lemmas** `lemma l[A](in x: A, …, out y: B, …) := requires a, … ensures b` with the modes of a procedure in the head, where ins are universal and outs existential, `ref` is rejected, and a proof follows the contract directly (section 4); a lemma without a proof has the default proof. `CLemma` of `ulang/syntax/syntax.u`, `parse_lemma` and `parse_proof` of `ulang/parse/parse.u`, `check_proof` of `ulang/check/check.u`.
- **The prover** (`ulang/prove/`): an e-graph with congruence closure (`egraph.u`), contextual simplification of nested sequents with theories, rules, and internal splits (`simp.u`), the equations of definitions (`equations.u`), termination proofs (`termination.u`), the checking of proofs (`proof.u`), the translation of checked programs (`translate.u`), and the default proof and the driver of lemmas (`lemma.u`).
- **Commands**: `ulang.sh prove [<path>[:<lemma>] …]` prints `lemma l proved` or `lemma l open`, with an error of the kind `proof` per gap, a JSON line at the step with the goal as a detail (`gap_diagnostic`), and one per function whose termination is not proven (`termination_diagnostic`); a named lemma is proven after those before it, which are its facts, and those after it are skipped. `ulang.sh test` tests each lemma on enumerated values (`l.lemma`, which claims do not get) and then proves it. `make termination` reports the recursive functions of `ulang/` whose termination is not proven. The VS Code extension shows `▶ Prove` above lemmas and the errors of proofs as warnings at their steps; after a change of `vscode/`, `make install` again.
- **Tests**: the lemmas that prove are in `ulang/prove/test/` (`arithmetic.u`, `lists.u`, `logic.u`, `naturals.u`, `trees.u`, `proofs.u`, `procedures.u`, and `contracts/`), those that stay open, by intent or as a guard of soundness, are cases of `ulang/test/errors/lemmas.u`, also the claims `claim_*` that stay open, and the errors of proofs and claims are cases of `ulang/test/errors/parse.u` and `types.u`.
- **Contracts of procedures** `requires a, …` and `ensures b` after `:=`, on the signature in `export` of an exported procedure, and **claims** `lemma l(in x: A, …, out y: B, …) := correct p(in a, …, out y, …)`, whose proof is an `invariant e` per `while` of the body in the order of the source, each over the variables at its loop; none is the invariant `true`. The claim does not restate the contract, and invariants are not in the code (section 2). The translation runs the body symbolically into the proof terms of section 4 (`claim_of`, `run`, `run_loop` of `ulang/prove/translate.u`): assignments are definitions, `if` and `match` splits that carry the rest of the body, calls the calls of the contracts, which proofs of claims may call at any time, also recursively (section 3, point 3), `raise` the call of a contract `ensures false`, and loops the split `InvariantTerm`, checked by `invariant_gaps` of `ulang/prove/proof.u` (section 3, loops, point 1). Claims are no facts and no lemmas to call, since they hold only for runs that end. The simplifier is unchanged.

Not implemented:

- **Of claims**: `old(x)` of refs and claims of procedures with refs (open point 10); `for`, `try`, and `assert raises` in bodies, which are gaps `Unsupported`; loop contracts (section 3, loops, point 2); other steps than invariants in proofs of claims, and the points `at …` of the code (open point 1); definedness (section 3, point 6); claims of procedures of other packages; tests of claims by running the procedure, which may raise or not end; the gaps that the prover leaves in claims, `via_direct` and the printing of goals, are Verification 5 and 8 of `ideas/verification.md`.
- **Otherwise**: contracts of functions, quantifiers in statements, exported lemmas, and the proof forms of version 2.

## 2. Specifications (implemented for procedures, a design for the rest)

- **Contracts follow `:=`**, before the body. Functions have only `requires`, which states their domain, and facts about their results are lemmas. Procedures have `requires` and `ensures`, one contract each for now, where `ensures` sees the outs and refs, and `old(x)` is the initial value of a ref (ins cannot be assigned):

  ```
  function div(m: Int, n: Int): Int :=
    requires n != 0
    ...

  procedure sum(in n: Int, out s: Int) :=
    requires n >= 0
    ensures  2 * s == n * (n - 1)
    local i := 0

    assign s := 0
    while i < n do
      assign s := s + i, i := i + 1
    end
  ```

- **Quantifiers** are written like lambdas, `forall x: T => e` and `exists x, y => e`, where the types may be omitted and are inferred, and only occur in specifications and lemmas; the functions `exists` and `forall` of the containers share the names (open point 15).
- **Lemmas** are implemented (section 4) with ins and outs as modes. The claim `correct p(in a, out b)` with the arguments of a call is implemented for procedures: a lemma whose statement is that the call meets the contract of `p`.
- **Outs of lemmas** are implemented: a lemma with outs states that for all ins that satisfy the `requires` there are outs that satisfy the `ensures`; its proof gives them by `assign`, and a proof that uses it binds them to witnesses by the outs of a `call`. In a claim `correct p(…)` the outs of the lemma are those of the call, which the run determines, not the proof: under partial correctness the claim holds for the outs of every run that ends normally, and under total correctness this is exactly the existential, since a run exists and gives them.
- **Scopes are explicit.** A proof sees only names that it or its lemma binds: the formals of the lemma, which name every argument of the call in the claim, and at a point `at …` of the body its locals in scope there, which the quoted code names. The names of the lemma may differ from the formals of the procedure, whose contract is renamed to them.
- **No invariants in the code.** Loops are specified in the proofs of the procedures, see below.

## 3. Meaning, obligations, loops, and points of the code (in part implemented)

**Meaning.**

1. **`correct p`** states for all ins and initial refs: if the `requires` hold, every run of the body that ends normally satisfies the `ensures`. The claim does not restate the contract.
2. **Calls use contracts**: at `call q(...)`, the `requires` of `q` are proven, the outs and refs havocked, and the `ensures` assumed. Callers depend only on the contract, not on whether or how it is proven. The `call` of a lemma in a proof is the same rule over values, which is why proofs write it so.
3. **Recursion.** Each body is proven assuming all contracts, including its own and those of procedures that call it back, which is sound for partial correctness for all of them at once by induction on the depth of calls (Hoare's recursion rule, generalized to mutual recursion). Proofs have no order and no cycles. Lemmas, unlike this, are proven in their order, and a lemma calls only those before it and itself as an induction hypothesis.
4. **Unproven contracts** make this conditional: if the contract of `q` is not proven, the proofs that use it hold only under it, which `ulang verify <file>` reports, e.g. `sum proved, assuming q`.
5. **Every contract is an obligation.** Without a `correct` lemma, the verifier tries a default proof and reports the remaining goals, including those at loops.
6. **Definedness**: calls of functions with `requires`, `/`, `%`, and `raise` give obligations wherever they occur, taking guards into account, as in `n != 0 ==> m / n > 0`.
7. **Functions terminate**, which the prover proves for the recursive functions that the lemmas of a module reach, and only those unfold (section 6); `decreases e` after `:=` is planned as the fallback measure (Checker 2 of `ideas/checker.md`).

**Obligations.** The verifier splits `correct p` into labeled obligations with positions, which errors show and proofs refer to: the `requires` of calls and the definedness conditions, every `assert` of the code, which then also holds as a fact, for every loop those of its invariant or contract (entry, preservation or step, and use), and the `ensures` at the end of every path through the body, where a `raise` ends a path that `ensures` does not cover, unless a handler of a `try` catches it.

**Loops.** The proof of a procedure gives each loop an invariant or a loop contract.

1. **Invariant `I`**: it holds on entry, `I` and the test lead through the body to `I`, and `I` and the negated test hold after the loop, where only the variables the loop assigns are havocked.
2. **Loop contract `requires P ensures Q`**: a loop is the recursive procedure `if b then body; call loop(ref vs) end` over the variables it assigns, and the contract is its contract, proven by assuming it for the remaining iterations. `P` holds at every test of the loop, and `Q` relates that state, as `old(v)`, to the state at the exit: exit, `P` and not `b` imply `Q` with `old(v)` as `v`; step, from `P` and `b` the body leads to a state where `P` holds and `Q` up to the exit implies `Q` from before the body; use, `P` holds on entry, then the assigned variables are havocked and `Q` assumed with `old(v)` as the values on entry.
3. **Invariants are a special case**, `requires I ensures I and not b`. An invariant relates the start of the loop to the current state, which may need ghost variables, while `Q` relates the current state to the end, for which `old` suffices: reversing a list into `ys` has the invariant `rev(xs) ++ ys == rev(xs0)` with the initial `xs0`, but the contract `ensures ys == rev(old(xs)) ++ old(ys)`.

**Points of the code in proofs.** A proof of `correct p` refers to the loops, branches, and calls of the body: by the order of goals (KIV, KeY), by matching the code, quoted modulo positions, `at while i < n invariant …`, by labels in the code, or by proof outlines. Matching the code, with the order as the fallback, is the candidate. The steps of section 4 are what such a point holds.

References: G. Ernst, Loop Verification with Invariants and Contracts, VMCAI 2022; T. Tuerk, Local Reasoning about While-Loops, VSTTE 2010; E. Hehner, Specified Blocks, VSTTE 2005; A. Charguéraud, Characteristic Formulae for the Verification of Imperative Programs, ICFP 2011; C. A. R. Hoare, Procedures and Parameters: An Axiomatic Approach, 1971; T. Nipkow, Hoare Logics for Recursive Procedures and Unbounded Nondeterminism, CSL 2002.

## 4. The proof language (implemented, version 1)

**Principles.**

- **Simplification does the routine work.** The goal is simplified after every step and at the end of every block, with every fact in scope and the lemmas proven before, so a proof only says what the simplifier does not find: which split, which lemma at which arguments, which intermediate fact, which witness. A written case that simplification would close is allowed, so that proofs survive a stronger simplifier.
- **A tree of blocks, not a tactic script.** A block has one goal; its steps add facts and witnesses, and at most one split, its last step, replaces the goal by its cases, each a block of its own. No goal stack, no focusing, no step that leaves goals for later, so an open goal is reported at the block where it stays open.
- **Steps are statements.** `local`, `call`, `assert`, `assign`, `match`, and `if` mean in a proof what they mean in a procedure, over values: a lemma is used as a procedure with a contract is called, a cut is an assertion, and the outs of a lemma are assigned. The only keyword of proofs is `induction`.
- **No nesting of declarations.** No lemmas inside proofs and no proofs inside cuts; what needs a proof of its own is a lemma at the top level, called where it is used.
- **Facts are not named**: simplification uses every fact in scope, and the facts of a proof are ground, so a name would only cite what is used anyway. Names come back with quantifiers (version 2).

**Syntax.** The proof follows the contract, as a body follows the contract of a procedure:

```
lemma take_drop[A](in n: Int, xs: List[A]) :=
  ensures take(n, xs) ++ drop(n, xs) == xs
  induction xs
    case List(x; rest) =>
      call take_drop(in n - 1, rest)
  end

lemma halves(in m: Int, out q: Int, r: Int) :=
  requires m >= 0
  ensures  m == 2 * q + r and 0 <= r and r < 2
  assign q := m / 2, r := m % 2

lemma remainder_of_halves(in m: Int) :=
  requires m >= 0
  ensures  m % 2 < 2
  local q: Int, r: Int

  call halves(in m, out q, r)
  assert r < 2
```

`Proof(steps: List[At[Step]], split: Option[At[Split]])` of `ulang/syntax/syntax.u`, a datatype of its own, so that a split is last by its type; `parse_proof` ends a proof at a token that starts no step, and `format` prints the steps as the statements of a body (`proof_text` of `ulang/format/format.u`).

| Step | Meaning |
|---|---|
| `local x := e` | a definition: a fixed variable `x` with the fact `x == e`, which an open goal names where it has `e` |
| `local q: T` | a witness, bound by an out of a `call` |
| `call l(in a, …, out q, …)` | the instance of the lemma `l`: its `requires` must simplify to true, else each is a gap, and its `ensures` is then a fact, at the witnesses for its outs; `l` is a lemma proven before, or the lemma itself as an induction hypothesis |
| `assert e` | a cut: `e` must follow by simplification, and is a fact afterwards |
| `assign q := e, …` | the witnesses of the outs of the lemma, each once, on every path, also in the cases of a split |

| Split | Cases |
|---|---|
| `induction x case p => … end` | on a formal of a datatype: a case per constructor, with the fields as variables and the hypothesis for each recursive field, generalized over the other formals; on a formal of type `Int`: the case `n < 0`, which must close, `case 0`, and a catch-all for `n > 0` with the hypothesis for `n - 1`; the hypothesis is called at any argument that the facts make the field or `n - 1` |
| `match e case p => … end` | a case per constructor of the type of `e` with the fact `e == p`, or per literal with the negations of those before |
| `if c then … else … end` | `c` and `not c`; the `else` may be left out if that case closes |

Cases that are not written must close by simplification. Patterns of cases may be nested and have guards: a split has a case per leaf of its patterns (section 9), in which each case that matches the leaf is checked, under its guard, and the cases after a guarded one under its negation; induction on an integer takes only `case 0` and a catch-all.

**Checking** (`check_proof`, `proof_step`, `proof_split`, `proof_cases` of `ulang/check/check.u`): the steps as statements over the formals and the locals before, a call against `Sig.lemmas` with the type parameters instantiated, the witnesses unassigned until a call binds them, the outs of the lemma assignable once and assigned on every path, joined over the cases of splits, where constructors that the cases do not cover count as paths that do not; the errors `UnknownLemma` and `NotInducible`, and those of statements.

**Proving** (`check_proof` of `ulang/prove/proof.u`): the translation gives the proof as a `ProofTerm` over the variables in scope as indices (`proof_term` of `ulang/prove/translate.u`); the walk keeps the fixed variables as levels, the levels of the variables of the terms, the facts, the goal, and the outs not yet assigned, which are existential in the goal, and gives the gaps, each an `OpenGoal` with the goal printed by `printable`, an `UnprovenCall`, or `Unsupported`, at its position.

**Version 2, deferred**: quantifiers in statements with named facts and their instantiation `h(a)`; `obtain` of existential facts other than lemma outs; facts used only when cited (open point 8); `calc` chains (open point 5); proofs of procedures, where the same steps occur at the points of the code (open point 1).

## 5. Simplification (implemented)

No external provers: simplification is one procedure in ulang, in `ulang/prove/simp.u`, which turns a goal into a single goal equivalent to it, given the facts, or closes it. Every step preserves equivalence, so simplifying never turns a provable goal into an unprovable one; case splits are made internally, kept only if all branches close; results are deterministic, and failures show the simplified goal. This is weaker than SMT, which is why proofs make the structure explicit.

**Sequents.** Goals, facts, lemmas, and induction hypotheses are nested sequents

```
forall xs. A1 and … and Ak ==> (exists ys1. C11 and … and C1m) or … or (exists ysj. Cj1 and …)
```

`Seq(fixed, assms, concls)` and `Concl(vars, body)` of `ulang/prove/simp.u`, where `false` is a sequent without conclusions and `not A` is `A ==> false`. Normalization moves the connectives, quantifiers, and `if` on `Bool` into this structure, again after unfolding, splices conjunctions and existentials of assumptions into the binders, and eliminates variables by the one-point rule. A conclusion that is a negation `not A`, as `a != b` is, is the assumption `A`, also in a disjunction, since `not A or R` is `A ==> R`, and the sequent is simplified once more with them, so that the equations of disequalities are facts for all other parts (`negated_conclusions`); comparisons stay in conclusions, which the arithmetic refutes anyway. Comparisons are never negated: `<=`, `>`, and `>=` of the prelude are builtins of the prover (`LessEq`, `Greater`, and `GreaterEq` of `ulang/prove/syntax.u`), `>` and `>=` are `<` and `<=` with their sides swapped, and a negation moves into the comparison, `not a < b` to `b <= a`, which holds for any total order; only `!=` stays a negated `==`.

**Terms.** The prover works on its own terms, `Term` of `ulang/prove/syntax.u`, translated from checked `Expr`s: indices for binders and levels for the variables of the context (Kovács' elaboration-zoo), names as the checker resolved them, described by `Decls` (definitions with their equations and the definitions they reach, constructors, datatypes, ranks, abstract types, ordered types, and the terminating functions), builtins as the names of checked programs (the aliases `Eq`, `Less`, `And`, `Or`, `Not`, `Plus`, `Minus`, `Times`, `Div`, `Mod`, `Compare`, `Size`), and canonical linear sums `ESum`.

**Contextual simplification.** Each part of a sequent is replaced by one equivalent to it in its context: the assumptions before it, the conjuncts before it in a conclusion, and the negated conclusions before it. Siblings see only those before them.

**The context** (`Ctx`) holds the variables, the e-graph of the ground equations, the inequalities `t >= 0`, the rules, and what is left of the limits (`Limits`: merges per fact, probe merges and probes, splits, unfoldings and rewrites nested, eliminations and constraints of Fourier–Motzkin, and the rounds and nodes of saturation, as one record).

- **The e-graph** (`ulang/prove/egraph.u`): nodes interned over the classes of their arguments, a union-find whose larger class stays the representative, use lists, and congruence by rehashing the parents of a merged class, as persistent maps, so that a nested sequent extends the e-graph of its parent and splits need no undo. It knows no theory: `unite` reports the classes that congruence makes equal, which the simplifier merges by the theories (`join`, `merge_classes`), and the parents of a merged class, which it simplifies again (`resimplify`), so that `size(xs)` reaches `1 + size(ys)` after `xs == List(y; ys)`. A term is rewritten to the best term of its class (`extract`: a literal, else a value, else a sum, the solved form of an atom, else the least in the term order), which is how facts reach their uses.
- **Theories**: constructors are injective, distinct, and acyclic, except those of abstract types, whose representation `==` need not respect (`free_head`); linear integer arithmetic solves an equation for its greatest atom with a unit coefficient, else keeps it divided by the gcd, and decides inequalities by Fourier–Motzkin with tightening, giving entailed equalities back.
- **Rules**: assumptions with variables, among them the lemmas proven before, applied first syntactically and then modulo the e-graph (`instance`, by `ematch`), with their conditions proven; equations that orient by the term order rewrite, others take part in a saturation that decides equations (`is_equal_by_rules`: a copy of the e-graph saturated with the unconditional equations in both directions for a few rounds, so that commutativity is usable).
- **Internal splits**, kept only if all cases close, within the split limit: on the conditions and matches of the sequent, then on the first disjunct of a disjunctive assumption, then on the variables of type `Bool` that the sequent mentions, then on the variable on which the unfolding of a recursive function is stuck (at cost 2).
- **Default proof** (`ulang/prove/lemma.u`): simplification, else induction on one of the formals of a datatype, with the lemmas proven before as facts, added after induction.
- **Printing**: an open goal is simplified once more, where a call of a function that is not recursive keeps its name if the match or if at the top of its body stays undecided (`for_printing`), so that it shows `maximum(a, b)` instead of its body, while proofs unfold every such call (`lazy_goal` of `ulang/test/errors/lemmas.u`); then it is folded back into the functions that are not recursive whose bodies it has, where that is smaller (`printable`: a short saturation of an e-graph of each term with the bodies as patterns, and the smallest term of its class), so that it reads as written, `reverse(xs)` for `reverse_onto(xs, [])` and `27 <= x` for `not x < 27`. Only the texts of open goals depend on it.

References: G. Nelson, D. Oppen, Simplification by Cooperating Decision Procedures, TOPLAS 1979; R. Shostak, Deciding Combinations of Theories, JACM 1984; S. Conchon, E. Contejean, J. Kanig, S. Lescuyer, CC(X), ENTCS 2008; M. Willsey et al., egg: Fast and Extensible Equality Saturation, POPL 2021; W. Pugh, The Omega Test, 1991; D. Miller, Higher-order patterns, 1991.

## 6. Definitions, rewriting, and termination (implemented)

**Equations of definitions** (`def_equations` of `ulang/prove/equations.u`): one per leaf of the patterns of a match on a variable or on a tuple of distinct variables (section 9) and per case that matches the leaf, under its guard and the negated guards of the cases before, and per branch of an if; the rest is a leaf, decided where it is still a match or if.

**Unfolding** (`unfold` of `ulang/prove/simp.u`): a function that is not recursive unfolds always; a recursive one only if its termination is proven, by the first of its equations that matches modulo the e-graph and whose conditions hold, else where the match or if of its body is decided, which leaves the conditions for splits; a recursive function whose termination is not proven stays uninterpreted, since unfolding `f(x) = f(x) + 1` would prove false. Nested unfoldings are limited to 256.

**Termination** (`terminations` of `ulang/prove/termination.u`), proven by the prover, not the checker: for the components of the recursive functions that the lemmas of a module reach, the recursive calls with the conditions around them, also in the cases of matches, by the leaves of their patterns, and under the lambdas of the folds, `exists`, and `forall` of the containers of the library, where the element is smaller than the container; measures are the builtin `size` of the formals of datatypes (0 for a constructor without fields, else 1 and the sizes of all fields, 0 for literals, with `size(xs ++ ys)` and `size(T.f(x)) < size(x)` for accessors), the Int formals, their negations, absolute values, and differences, so that `gcd` terminates by its divisor, the differences of the comparisons in the conditions of a body, and for mutual recursion the sums of sizes and slots of positions; a lexicographic order is searched greedily, as Isabelle's `lexicographic_order`, each decrease proven by the simplifier. A component that does not terminate is an error of the kind `proof` at the call that no measure decreases, and its functions stay uninterpreted. Of the library, `distinct`, `separated`, `sort` (which need lemmas of sizes), and `list_count` and `list_values` of `ulang/test/enumerate/enumerate.u`, which the cases load with the library (which count down to `n == 0` and do not terminate for a negative `n`) are open (`library_terminates` of `ulang/test/errors/lemmas.u`); `make termination` lists 82 open functions of `ulang/`.

## 7. Decision procedures

| | Procedure | Status |
|---|---|---|
| D1 | `compare` and `<` on values: integers, booleans, tuples, constructor terms of datatypes ordered by structure | done; strings and characters stay atoms |
| D2 | `compare` by the context, constructors of abstract types not free | done |
| D3 | ground clauses: splits on disjunctive assumptions | done, by splitting, without unit propagation |
| D4 | `Bool` as a finite type: splits on its variables | done; other finite datatypes not yet |
| D5 | constructor exhaustiveness: `xs != List()` gives the other constructor | partly, by stuck splits |
| D6 | `/` and `%`: the equation, bounds, and sign | done, by literals, and by a variable divisor where the facts give that it is not 0, with the bounds where they give its sign |
| D7 | integer completeness: the Omega test, or branch and bound | open; `integer_infeasible` |
| D8 | commutative rings: products as monomials, squares at least 0 | done |
| D9 | induction on integers | done, in proofs |

## 8. Next steps

In the order I would take them, each with the problem, the solution, and where.

1. **Unfoldings as unions**, the rest of B4 of the former plan, *not needed now* (2026-11-12): no goal was found that it would close, since facts enter the e-graph and their parents are simplified again after merges, and conclusions are simplified after all facts; the lazy unfolding that it was to serve only prints (section 5), so it needs none. Take it up with a goal that fails for the order of unfolding. *Problem*: unfolding happens in `simp_term`, which rebuilds terms bottom-up and returns them, so the unfoldings of the goal's terms never enter the e-graph, whose parents are simplified again after a merge only for the terms of facts: results depend on the order of unfolding, and printing needs an e-graph of its own. *Solution*: thread the context through `simp_term`, so that each unfolding and rewrite `t → u` is a union by `join`, with the theories and the parents simplified again, where the threading stops wherever the facts change, in the branches of an if, trial merges, and nested sequents; extraction ranks an unfolded application after its unfolding, else a class would give back the call (as for the definitions of proofs, `term_rank`); then the unconditional equations of definitions as rules matched against the classes of the e-graph of the context within the node budget, then the conditional ones with their conditions proven by the simplifier; `printable` extracts from the e-graph of the context. No memo: *measured* (2026-11-12), `simp_term` threading the context and keeping the normal forms of the terms simplified since the facts or the budget of merges last changed proves the same lemmas, and over five runs of `ulang/prove/test/` takes `arithmetic.u` from 366 to 503 ms and `lists.u` from 447 to 386 ms, the memo of applications only 463 and 376 ms, of defined functions only 453 and 451 ms, and the threading alone 406 and 457 ms, since the memo lives only between two facts; reverted. *Considered and rejected*: sequents whose terms are classes of a shared e-graph, since e-graphs over binders are known to be tricky, and the indices and levels of the sequents and the binders of lambdas, matches, and conditionals would have to enter the classes. *Limit*: unions reach only what the equations of definitions cover (`def_equations` of `ulang/prove/equations.u`); a body that is still a match or if after them, a decided leaf, a match on a computed value, or a call under the lambda of a fold, is unfolded by the walk of the simplifier (`decided_body`, `select`), which stays, so the order of unfolding stays for those, and the coverage of definitions by equations is what makes unfolding complete. *Risk*: the goal's terms become parents that every merge simplifies again, which may cost more than the threading; the budget of merges then has to cover them. *Where*: `ulang/prove/simp.u`, `egraph.u`. To be measured.
2. **Termination of the rest of `ulang/`.** *Problem*: 82 functions are open in `make termination`, mostly passed as values or called under lambdas of other functions, recursion on filtered lists, and recursion on results of functions. *Solution*: calls through functions passed as values, by their uses; lemmas of sizes (`size` of a filter, of `take`) as facts of the obligations; case splits in obligations on results such as `shape(t)`; and `decreases` for the rest (Checker 2 of `ideas/checker.md`), until `make termination` can become a check of `make test`.
3. **The Omega test** (D7). *Problem*: Fourier–Motzkin misses `27 <= 11x + 13y <= 45, -10 <= 7x - 9y <= 4`. *Solution*: the dark and grey shadows of Pugh, or branch and bound on the rational solution. *Where*: `is_unsat` of `ulang/prove/simp.u`.
4. **Arithmetic over classes** (B3). *Problem*: the inequalities are terms, simplified again after each union. *Solution*: sums and inequalities over class ids, entailed equalities fed back to the e-graph. *Where*: `simp.u`, `egraph.u`.
5. **Exhausted limits in reports**: Verification 7 of `ideas/verification.md`.
6. **Version 2 of the proof language**, deferred (section 4).
7. **Contracts of functions and exported lemmas** (section 2; Verification 1 of `ideas/verification.md`): with abstract types uninterpreted for lemmas of other modules.

## 9. Proposals

**Definitions kept as names** (implemented). A variable of `local x := e` is marked as defined (`defined_var` of `ulang/prove/syntax.u`, a module that no program has, so that the mark survives rebinding), and the simplifier keeps it: `one_point` does not eliminate it, the arithmetic solves an equation for it before any other atom (`is_solved_before`), so that it stays equal to its value, and extraction takes it last (`term_rank`), so that no term is rewritten to it and the rules still see its value; reasoning sees `e` wherever `x` was, as after the elimination. Printing (`named_definitions` of `ulang/prove/simp.u`) folds the value of each defined variable, as its equation in the simplified sequent has it, into its name, the last definition first, so that a value inside another one folds after it, and leaves out the equation and the variable where it no longer occurs (`proof_definitions_named` of `ulang/test/errors/lemmas.u`). *Open*: a value that the simplifier combines with other terms, as `x + 1` in a sum, is printed expanded, since folding is syntactic.

**Nested patterns and guards in splits** (implemented). A case may cover only part of a constructor: after `case List(x, y; rest)`, a case `List(x; rest)` means `Cons(x, Nil)`. `leaves` of `ulang/prove/patterns.u` compiles the patterns of a match into a decision tree (Wadler 1987, Maranget 2008), as the exhaustiveness check of the checker, `missing` and `is_useful` of `ulang/check/check.u`, specializes the rows: the first column is split on the constructors of its datatype where a row has a constructor, a row with a variable or wildcard going to every branch, on a literal into the literal and the rest with its negation, and else dropped, where a first row of variables without a guard ends the split. A leaf is a value over new variables, named as the first row that binds them or else as their fields, with the conditions of the literals and the rows that match it, up to the first one without a guard, each with the values of its variables. Proofs check each case once per leaf that it matches (`leaves_gaps` of `ulang/prove/proof.u`), with the equation of the leaf as a fact, the guard as a fact of its case and negated for the cases after it, and a leaf without a case must close; induction splits the formal on all its constructors, also where only catch-alls are written, and has the hypotheses for the recursive fields of the constructor of the leaf. The equations of definitions are one per leaf and case (`leaf_equations` of `ulang/prove/equations.u`), also of a match on a tuple of formals, and the termination analysis takes the leaves for the matches in bodies (`leaf_calls` of `ulang/prove/termination.u`), with the negated guards of the cases before. *Open*: a case checked at several leaves is reported at each one that it leaves open, without the leaf in the message; `expand` and `hole_term` of `ulang/prove/patterns.u`, `hits_equations`, and `hits_gaps` are not proven to terminate.

## 10. Open points

1. **Proofs of programs**: proofs of claims are invariants by the order of the source; open are the steps inside the points `at …` of loops and calls (Verification 3 of `ideas/verification.md`), and which of the ways to refer to points of the code.
2. **Arithmetic**: settled for now, Fourier–Motzkin with tightening, with the Omega test as a next step (section 8, step 3).
3. **Orientation**: rewrite rules only from left to right, or also `using <- l`; settled for equations that do not orient, which saturation uses in both directions.
4. **More induction**: settled for integers; computation induction `induction f(xs, ys)`, whose cases follow the `match` of `f`, is open.
5. **`calc` chains** for equational proofs, which cuts cover for now.
6. **Names**: settled, the keywords `induction`, `correct`, and `invariant`, and the statements as steps.
7. **Partial instantiation** `h(a, _)`, with version 2.
8. **Facts kept from simplification**, used only when cited.
9. **Unfolding functions that are not recursive** always may break abstractions, e.g. `max`; open goals keep their calls where the body stays undecided (section 5), and printing folds the rest back.
10. **Refs in claims**: a ref of the procedure as an in for its initial value and an out for its final value.
11. **Contracts without a `correct` lemma**: tried by a default proof, or reported as unproven.
12. **Several contracts per procedure**, e.g. as claims `correct p ensures …`.
13. **Total correctness**, by `decreases` on loops and procedures, and claims about exceptions.
14. **Runtime checks** of contracts in the interpreter.
15. **Quantifiers and the functions `exists` and `forall`** of the containers share their names; decided: bounded quantifiers replace the functions (Syntax 1 of `ideas/syntax.md`), and the unbounded `forall x: T => e` occurs only in specifications.
16. **Splitting on ground clauses**: settled by internal splits on disjunctive assumptions.
17. **Theories as values or tags**: a tag dispatched by `match`, as now.
18. **Equations between functions** among the assumptions, which need extensionality.
19. **Speed**: the e-graph is persistent maps with interned nodes; a memo of the simplified terms, threaded through simplification, did not pay (section 8, step 1).
