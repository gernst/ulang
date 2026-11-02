# Design

## Design notes

What a language would ideally offer for writing code with an AI assistant, which types cheaply but makes mistakes it cannot see: implicit conversions, shadowed names, hidden effects, forgotten cases.

1. **Everything that matters is visible where it happens**: effects and data flow in signatures, no implicit conversions or overloading by type, and failure in the type, with short propagation like Rust's `?`.
2. **A strict, fast, and precise checker**: inference within functions, types at module boundaries, exhaustive matches, and errors with positions and expected types, since the checker is the feedback loop.
3. **Small and uniform**: few concepts, one way to write each, no context-dependent syntax, and a canonical formatter.
4. **Modules with explicit exports, and records with named fields**, since names are how code is navigated, and clashes and positions are where mistakes happen.
5. **Abstraction over effectful code**, such that patterns like `repsep` are written once.
6. **Specifications in the language**: pre- and postconditions and properties, checked by tests or a prover, since the common mistakes are subtle logic errors rather than type errors.

### Compared to ulang

- **Already there**: explicit data flow with `in`, `out`, and `ref`, pure expressions with effects only in statements, exhaustive matches and inference, a small language without implicit behavior, exceptions as values of a type, named tests with `assert` showing both sides, and procedures as values, which abstract over effectful code such as the parsing of sections in `self/parse.u`.
- **Missing**: modules with exports, positions in the errors of the self-hosted checker and compiler, failure in the types, and contracts, while records exist in part, as named fields with accessors and updates. These are extensions, not redesigns, see `TODO.md`.
- **Questionable**: user-defined notations make the syntax depend on declarations, e.g. whether `and` is an operator or a name, for tools and for writing code alike.

## Verification

A design, not implemented yet: contracts on functions and procedures, lemmas about functions, and claims `correct p` that a procedure meets its contract, whose proofs stay out of the code.

### Specifications

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
    while i < n
      assign s := s + i, i := i + 1
    end
  ```

- **Quantifiers** are written like lambdas, `forall x: T => e` and `exists x, y => e`, where the types may be omitted and are inferred, and only occur in specifications and lemmas.
- **Lemmas** have parameters like functions, which are universally quantified, and optionally outs as a tuple at the end, `: (q: Int, r: Int)`, which are named, unlike the components of tuple types, and existentially quantified. A lemma has a contract like a procedure, `requires` as hypotheses and `ensures` as the statement, or the claim `correct p(…)` with the arguments of a call, followed by a proof:

  ```
  lemma length_append[A](xs: List[A], ys: List[A]) :=
    ensures length(xs ++ ys) == length(xs) + length(ys)
  proof
    ...

  lemma division(m: Int, n: Int): (q: Int, r: Int) :=
    requires n > 0
    ensures  m == q * n + r and 0 <= r and r < n
  proof
    ...

  lemma sum_correct(n: Int): (s: Int) :=
    correct sum(in n, out s)
  proof
    ...
  ```

- **Outs of lemmas.** A lemma with a contract states that for all parameters that satisfy the `requires` there are outs that satisfy the `ensures`, so that its proof gives witnesses, which a proof gives by `choose q := m / n, r := m % n`, and a proof that uses the lemma obtains them with names of its own by `obtain (q, r) := division(a, b)`, see the proofs below. In a claim `correct p(…)` the outs of the lemma are those of the call, which the run determines, not the proof: under partial correctness the claim holds for the outs of every run that ends normally, and under total correctness this is exactly the existential, since a run exists and gives them.
- **Scopes are explicit.** A proof sees only names that it or its lemma binds, never the parameters of the procedure implicitly:
  - the parameters and outs of the lemma, which name every argument of the call in the claim, `correct p(in n, out s)`,
  - at a point `at …` of the body, its locals in scope there, which the quoted code names, e.g. `i` in `at while i < n`.

  The names of the lemma may differ from the parameters of the procedure, whose contract is renamed to them, so that a proof does not change when a parameter is renamed.
- **No invariants in the code.** Loops are specified in the proofs of the procedures, see below.

### Meaning

1. **`correct p`** states for all ins and initial refs: if the `requires` hold, every run of the body that ends normally satisfies the `ensures`. The claim does not restate the contract.
2. **Calls use contracts**: at `call q(...)`, the `requires` of `q` are proven, the outs and refs havocked, and the `ensures` assumed. Callers depend only on the contract, not on whether or how it is proven.
3. **Recursion.** Each body is proven assuming all contracts, including its own and those of procedures that call it back, which is sound for partial correctness for all of them at once by induction on the depth of calls (Hoare's recursion rule, generalized to mutual recursion). Proofs have no order and no cycles.
4. **Unproven contracts** make this conditional: if the contract of `q` is not proven, the proofs that use it hold only under it, which `ulang verify <file>` reports, e.g. `sum proved, assuming q`.
5. **Every contract is an obligation.** Without a `correct` lemma, the verifier tries a default proof and reports the remaining goals, including those at loops, so that the workflow is to write the contract, see where it gets stuck, and add a lemma with a proof.
6. **Definedness**: calls of functions with `requires`, `/`, `%`, and `raise` give obligations wherever they occur, taking guards into account, as in `n != 0 ==> m / n > 0`.
7. **Functions in lemmas must terminate**, by structural recursion or a `decreases` after `:=`, since unfolding `f(x) = f(x) + 1` would prove false.

### Obligations

The verifier splits `correct p` into labeled obligations with positions, which errors show and proofs refer to:

- the `requires` of calls and the definedness conditions,
- every `assert` of the code, which then also holds as a fact,
- for every loop, those of its invariant or contract (entry, preservation or step, and use),
- the `ensures` at the end of every path through the body, where a `raise` ends a path that `ensures` does not cover, unless a handler of a `try` catches it.

### Loops

The proof of a procedure gives each loop an invariant or a loop contract.

1. **Invariant `I`**: it holds on entry, `I` and the test lead through the body to `I`, and `I` and the negated test hold after the loop, where only the variables the loop assigns are havocked.
2. **Loop contract `requires P ensures Q`**: a loop is the recursive procedure `if b then body; call loop(ref vs) end` over the variables it assigns, and the contract is its contract, proven by assuming it for the remaining iterations. `P` holds at every test of the loop, and `Q` relates that state, as `old(v)`, to the state at the exit:
   - **exit**: `P` and not `b` imply `Q` with `old(v)` as `v`,
   - **step**: from `P` and `b` the body leads to a state where `P` holds and `Q` up to the exit implies `Q` from before the body,
   - **use**: `P` holds on entry, then the assigned variables are havocked and `Q` assumed with `old(v)` as the values on entry.
3. **Invariants are a special case**, `requires I ensures I and not b`. `P` is an inductive invariant as well, but it need not imply the result, since `Q` carries that. An invariant relates the start of the loop to the current state, which may need ghost variables, while `Q` relates the current state to the end, for which `old` suffices: reversing a list into `ys` has the invariant `rev(xs) ++ ys == rev(xs0)` with the initial `xs0`, but the contract `ensures ys == rev(old(xs)) ++ old(ys)`, and `total` of `tests/procedures.u` has `ensures s == old(s) + sum(old(ys))`. Counting loops are usually simpler with invariants, loops that consume a structure with contracts.

References: G. Ernst, Loop Verification with Invariants and Contracts, VMCAI 2022; T. Tuerk, Local Reasoning about While-Loops, VSTTE 2010; E. Hehner, Specified Blocks, VSTTE 2005, and A Practical Theory of Programming, 1993; the recursive loop proofs of VeriFast; A. Charguéraud, Characteristic Formulae for the Verification of Imperative Programs, ICFP 2011; C. A. R. Hoare, Procedures and Parameters: An Axiomatic Approach, 1971; T. Nipkow, Hoare Logics for Recursive Procedures and Unbounded Nondeterminism, CSL 2002.

### Points of the code in proofs

A proof of `correct p` is about the body, but not in it, so it refers to its loops, its branches, its calls, where a lemma may bridge the contract, and to intermediate facts.

1. **By the order of goals** (KIV, KeY): symbolic execution stops at loops and branches, and the proof answers the goals in order, with `case` for branches. No names, but unreadable without the goals and fragile when code moves.
2. **By matching the code**: the proof quotes enough of it, matched against the body modulo positions, readable without tools and robust to most edits, ambiguous where a loop repeats, which falls back to the order or a path like `at if a > b / else / while …`:

   ```
   at while i < n
     invariant 0 <= i and i <= n and 2 * s == i * (i - 1)
   at call total(in ys, out t)
     using sum_append
   ```

3. **By labels in the code**, `while i < n as count`, stable, but in the code.
4. **By proof outlines**, a skeleton of the body with annotations, explicit, but it duplicates the code.

Matching the code, with the order as the fallback, is the candidate.

### Proofs of propositions

Proofs of lemmas with contracts, while proofs of programs are open, see above. Proofs are always structured, simpler than in Isabelle, and read like expressions, with `match` and `if` chains as in functions.

**Goals** consist of fixed variables (the parameters of the lemma, and those bound by cases and witnesses), named facts (the `requires`, case equations, `have`s, and induction hypotheses), and a conclusion. Facts may be named, see below, where names are only needed to cite them.

**Simplification is implicit.** Every goal is simplified before the proof sees it, at the start of a lemma, in every case and branch, and after every `have`, `using`, `obtain`, `choose`, `fix`, and `assume`, see below. Goals that simplification closes need no proof: their cases are omitted, and a sequence may end without a closing form if its goal is closed.

**Forms:**

| Form | Meaning |
|---|---|
| `using f, …` | adds facts, where `f` is a fact, a lemma, an instance `take_drop(n - 1, rest)`, or a function for its definition, which simplification then uses |
| `l(args)` | closes the goal by the instance of a lemma, whose `requires` are simplified to `true` or remain as subgoals |
| `have h := e`, `have h := e by p` | a named intermediate fact, proven by simplification or by `p`, available to everything after it |
| `match e case pat => p … end`, `match h := e …` | cases on a value, where each case gets the fact `e == pat` and its variables |
| `induction x case pat => p … end` | structural induction on a parameter of a datatype, with cases as in `match` |
| `if c then p else if d then p else p end`, `if h := c …` | a split on conditions, where each branch gets its condition and the negations of the earlier ones |
| `subgoals have h := e … end` | subgoals, each a `have` that sees the earlier ones, from which the goal follows by simplification |
| `obtain (q, r) := l(args)`, `obtain y := h` | uses a lemma with outs or an existential fact: proves the `requires`, binds the witnesses, and adds the `ensures` or the body of the quantifier |
| `choose q := e, r := e` | gives the witnesses for the outs of the lemma being proven, or for the existential of the goal |
| `fix y` | names the variable of a universal of the goal |
| `assume k := a` | names the premise `a` of an implication `a ==> b` of the goal |
| `h(args)` | instantiates a universal fact, wherever instances of lemmas go |

**Omitted cases.** Cases of `match` and `induction`, branches of `if` (`if c then p end` leaves the case `not c` to simplification), and subgoals that simplification closes are omitted, and those that remain are written out or covered by `else`. A case that is not closed and not written is an error that shows its simplified goal. Writing a case that simplification closes is allowed, so that proofs survive when simplification gets stronger.

**Sequences.** A proof is a sequence of `using`, `have`, `obtain`, `choose`, `fix`, and `assume`, optionally ending in `l(args)`, `match`, `induction`, `if`, or `subgoals`, and its goal must be closed at its end. The bodies of `case`, `then`, and `else` are such sequences, which end at the next `case`, `else`, or `end` like the bodies of statements, which is why the `if` of proofs has an `end`, unlike that of expressions. A `have` with a longer proof is `have h := e`, a sequence, and `end`, like `while`.

**Facts** are named by `:=`, like variables, wherever they arise:

| Where | Syntax | Fact |
|---|---|---|
| contract | `requires pos := n > 0` | the hypothesis |
| intermediate fact | `have h := e`, `have h := e by p` | `e` once proven |
| condition | `if h := c then p else if k := d then p else p end` | `c` in the `then`, `not c` in the `else` |
| match | `match h := e case pat => … end` | `e == pat` in every case, only needed if `e` is not a variable, which is substituted |
| subgoal | `subgoals have h := e … end` | `e`, for the later subgoals |
| implication | `assume k := a` | the premise `a`, checked against the goal modulo simplification |
| induction hypothesis | | the lemma itself, by its name, e.g. `take_drop(n - 1, rest)` |

`obtain` binds variables, not facts, and its fact is named by a following `have k := P(y)`, which simplification proves at once and which restates the formula where the proof needs it.

Facts are used:

1. **implicitly** by simplification, all that are in scope, as rewrite rules if they are equations that the term order orients, and else as conditions, such that `pos` closes side conditions `n > 0`, which is why most facts are never cited, and `have` is the usual way to steer a proof, by stating a step from which the rest follows,
2. **by instantiation** `h(a, b)` of universal facts, where instances of lemmas go, `using h(a, b)`, `have k := … by h(a, b)`, or as the closing form, where a universal is used implicitly only if it orients as a rewrite rule,
3. **by obtaining** witnesses, `obtain y := h`, of existential facts,
4. **as the closing form** `h`, which closes a goal that follows from `h` after simplification.

A fact is visible in the rest of its sequence and in its nested cases and branches, while facts of cases and branches stay in them, and the proof of a `have` sees only the facts before it. Names of facts are local like variables, may shadow lemmas like locals shadow functions, and must be distinct in a scope like those of `let`.

**Quantifiers** in goals and facts are handled like the parameters and outs of lemmas, which are a universal over the parameters and an existential over the outs:

| | `forall x => P` | `exists x => P` |
|---|---|---|
| fact, used | instantiated: `h(e)` | witness obtained: `obtain y := h` |
| goal, proven | variable fixed: `fix y` | witness given: `choose x := e` |

Simplification fixes the outermost universals and implications of the goal, and the outermost existentials of facts, by itself, with the names of the binders, since this loses nothing and is always desirable, so that `fix`, `assume`, and `obtain y := h` are only needed for names, or where a name clashes, in which case the variable is inaccessible and its use an error that asks for it. Instantiating universal facts and giving witnesses for existential goals are choices, after which a goal may no longer be provable, and stay explicit. `choose` takes the names of the binders of the goal, which stay stable as long as the goal does.

**Induction.** Its cases are the constructors of the type, with patterns as in `match`. The induction hypothesis is the lemma itself: in the case `[x; rest]` it may be used on `rest` with any values for its other parameters, e.g. `take_drop(n - 1, rest)`, so that these are generalized for free. The instance with the other arguments unchanged is a fact of the case, and other instances are added by `using`. Induction is only on parameters of datatypes, and the hypothesis only on variables of recursive arguments of constructors, the same structural check as for functions.

**Rewrite rules.** Lemmas declared by `rewrite lemma` are used by simplification wherever they are visible, by the rules of modules for other declarations. Their `ensures` is an equation `l == r`, used from left to right, or a condition `e`, used as `e == true`, their `requires` are conditions that simplification must prove before rewriting, the variables of `r` and of the `requires` must occur in `l`, and every rule must decrease in the term order, see below, which is checked at the declaration.

```
rewrite lemma append_nil[A](xs: List[A]) :=
  ensures xs ++ Nil == xs
proof
  induction xs
  end

rewrite lemma append_assoc[A](xs: List[A], ys: List[A], zs: List[A]) :=
  ensures (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
proof
  induction xs
  end

lemma take_drop[A](n: Int, xs: List[A]) :=
  ensures take(n, xs) ++ drop(n, xs) == xs
proof
  induction xs
    case [x; rest] =>
      using take_drop(n - 1, rest)
  end

lemma reverse_onto_append[A](xs: List[A], ys: List[A]) :=
  ensures reverse_onto(xs, ys) == reverse(xs) ++ ys
proof
  induction xs
    case [x; rest] =>
      using reverse_onto_append(rest, [x; ys]), reverse_onto_append(rest, [x])
  end

lemma division(m: Int, n: Int): (q: Int, r: Int) :=
  requires m >= 0, n > 0
  ensures  m == q * n + r and 0 <= r and r < n
proof
  choose q := m / n, r := m % n
```

`induction xs` without cases closes all of them by simplification. In `take_drop` the case `Nil` is closed, and in `[x; rest]` simplification splits on `n == 0` from `take` and `drop`, where both branches close, one with the cited instance. `division` requires `m >= 0`, since `/` and `%` truncate towards zero.

### Simplification

No external provers: simplification is one procedure in ulang, in `simp/`, which turns a goal into a single goal that is equivalent to it, given the facts, or closes it.

- **Safe**: every step preserves equivalence, so that simplifying never turns a provable goal into an unprovable one, and is applied without asking. Hence no heuristic instantiation of quantifiers, no induction, no generalization, and no facts are dropped.
- **Desirable**: it terminates, normalizes, and keeps one goal, so that the proof sees one predictable goal. Case splits are only made internally, to close a goal, and their branches do not remain when they do not all close.

This is weaker than SMT, which is why the language makes the structure explicit by cases, induction, named facts, and instances, but there are no timeouts, results are deterministic, and failures show the simplified goal.

#### Sequents

Goals, facts, lemmas, and induction hypotheses are all nested sequents

```
forall xs. A1 and … and Ak ==> (exists ys1. C11 and … and C1m) or … or (exists ysj. Cj1 and …)
```

whose assumptions `Ai` and conclusions `Cij` are again sequents or atoms, where each disjunct of the conclusion is a conjunction under its existential, mirroring the assumptions, since a conjunction under `exists` cannot be split into goals:

```
data Seq :=
  | Atom(Term)
  | Seq(fixed: List[Type], assms: List[Seq], concls: List[Concl])

data Concl :=
  | Concl(vars: List[Type], body: List[Seq])
```

- `false` is a sequent without conclusions, `not A` is `A ==> false`, and several goals, as of the cases of `match` and `induction` or of `subgoals`, are a conjunction in a single conclusion.
- **A lemma is a sequent**: `lemma l(xs): (ys) := requires A1, …, Ak ensures C` is `Seq(xs, [A1, …, Ak], [Concl(ys, [C])])`. Proving it opens it as the goal, using it, by `using l`, `rewrite lemma`, or as the induction hypothesis, adds it unopened as an assumption, `h(a, b)` instantiates its outer binders, and `obtain` opens the existential of an assumption.
- **Normalization** from `Expr` moves `and`, `or`, `==>`, `not`, the quantifiers, and `if` on `Bool` into this structure, again after unfolding, which brings back connectives. It makes the steps that preserve equivalence and lose nothing: existentials of assumptions become fixed, `(exists y. A) ==> C` to `forall y. A ==> C`, and the one-point rules `forall x. x == t ==> P(x)` to `P(t)` and `exists y. y == t and P(y)` to `P(t)`.
- **Quantifiers are constants applied to lambdas**, `forall x: T => e` is `forall(lambda x: T => e)` of type `(T -> Bool) -> Bool`, and several variables are nested lambdas, so that lambdas are the only binders of formulas, the types of the variables come with them, and `forall` and `exists` cannot be computed, which the checker allows only in specifications.

#### Terms

The prover works on its own terms, translated from checked `Expr`s, since it needs invariants that `Expr` does not guarantee, and nodes for the values of its decision procedures. It does not depend on the syntax of `self/`, so it lives in `simp/`, and the translations in `self/`:

```
data Term :=
  | Bound(Int)                       // de Bruijn index: a binder inside the term
  | Var(Int)                         // de Bruijn level: a variable of the context
  | Lit(Val)
  | Con(Sym, List[Term])
  | Fun(Sym, List[Term])             // declared functions and builtins
  | Apply(Term, List[Term])          // application of a term that is not a symbol
  | Lam(Hint, Type, Term)
  | Ite(Term, Term, Term)
  | Match(Term, List[Alt])           // the variables of the patterns are indices too
  | Sum(Int, List[Term * Int])       // c + k1·t1 + …: canonical linear sums
```

- **Indices for binders, levels for the context.** The binders of `Lam`, `Match`, `Seq`, and `Concl` are indices, so that terms with binders are closed and the same at every depth: definitions, lemmas, and rules are used anywhere unchanged, and terms that differ in the names of bound variables are equal. The variables of the opened sequents are levels, counted from the root, so that a term of an outer sequent is valid unchanged in the inner ones, and the state stored at a sequent is shared by those nested in it. Indices for everything would shift the stored states at every nesting, and levels for everything every closed term with binders that moves, e.g. a definition that is unfolded. Opening a binder, of a lambda or a sequent, pushes its variable onto the context and replaces its index by the level, and closing does the reverse, the only places where the two meet. The context keeps the types of the levels, and their hints for printing.
- **No meta variables**: an assumption that is used as a rule, or a lemma, is closed, and its own binders are the variables that matching instantiates, where a pattern variable applied to distinct bound variables is matched as a higher-order pattern (Miller). Meta variables would only be needed for witnesses that are instantiated later by unification, which proofs give explicitly by `choose` instead. Type parameters of lemmas are matched the same way.
- **Symbols are numbers**, so that terms compare as integers rather than as names, and a signature describes them, which the translation builds: the constructors of each datatype, the builtins, the definitions of functions as terms with whether they are recursive, the order of declarations for the term order, and the names for printing.
- **Sums are canonical**: sorted by `compare` on their atoms, without coefficients `0`, nested sums, or sums of a single atom with coefficient `1`, and a constant is a `Lit`, kept by a smart constructor, and as lists, since `==` on maps compares their trees. Products with constants are sums, products of variables are atoms.
- **Translations**, in `self/`: `let` is substituted, comprehensions become `flat_map` over lambdas, function definitions are translated once to lambdas over their parameters, so that unfolding is beta reduction, and the prelude's `not`, `==>`, `!=`, `<=`, `>`, `>=`, and `-` are unfolded to the builtins, and folded back when printing. Open goals are printed by translating back to `Expr`, with the hints as names, and `simp/` prints terms itself for its tests.

#### Contextual simplification

Each part of a sequent is replaced by one that is equivalent to it in its context, for all values of the variables bound around it, which is why simplifying under binders is sound.

- **The context** of a sequent is that of its parent, its fixed variables as new levels, its assumptions, each simplified in the context of those before it, and for its `i`-th conclusion, the negations of the conclusions before it that have no existential, since `A ==> C1 or C2` is `A and not C1 ==> C2`. Siblings are simplified in order and each with the simplified earlier ones only, since simplifying `A and A`, each with the other, would give `true and true`.
- **Assumptions** are used depending on their form, decided once when they are added: ground atoms go into the state of the decision procedures, ground clauses propagate their last undecided literal once the others are decided, and sequents with variables are rules: equations that the term order orients rewrite from left to right, with their assumptions as conditions that must simplify to `true`, and others close an assumption or conclusion that is an instance of them.
- **Closing and dropping**: a sequent is proven if its state is inconsistent or a conclusion simplifies to `true`, and assumptions that simplify to `true` and conclusions that simplify to `false` are dropped.
- **State in the sequents**: each opened sequent stores its context, `Ctx(cc: CC, rules: List[Seq], clauses: List[Seq])`. Since values are immutable, the state of a nested sequent extends that of its parent and shares it, and splits and proof steps need no undo: a `have` adds an assumption and simplifies the sequent again from the state of its parent, a `match` replaces a sequent by its cases, and when the context of a sequent changes, the states below it are recomputed.

#### Decision procedures: CC(X)

Congruence closure over terms, parameterized by Shostak theories, as in CC(X) of Alt-Ergo: each theory gives a canonizer, whose results are syntactically equal exactly when the terms are equal in the theory, and a solver, which turns an equation into a substitution or `false`, and treats the terms of other theories as atoms, its leaves. Solved equations are oriented substitutions, i.e. the rewriting with facts, and the canonical forms are what the proof sees in open goals.

- **The core** is a union-find with use lists and a table of signatures, as maps keyed by terms, which costs a logarithm per operation instead of nearly constant time, but makes the states persistent. Lambdas, applications of variables, and comprehensions are uninterpreted terms, with applications as a symbol `@` of the function and its arguments.
- **Constructors**: the canonizer is the identity, and the solver is unification with an occurs check, such that constructors are injective, distinct, and acyclic, `xs == [x; xs]` being `false`.
- **Linear integer arithmetic**: the canonizer gives `Sum`s, the solver solves an equation for an atom with coefficient `1` or `-1`, and otherwise introduces fresh variables, `2·x == 3·y` to `x == 3·k` and `y == 2·k`, as in the Omega test. Inequalities are not Shostak: Fourier–Motzkin with integer tightening decides them over the canonical sums, and gives the equalities it implies back to the core, as in Nelson–Oppen, where `m / n` and `m % n` come with `m == (m / n) * n + m % n` and their bounds.
- **Other theories**, e.g. finite sets, add nodes to `Term` for their canonical forms in the same way.
- Nelson–Oppen alone would only decide validity and give no normal forms, and splits for theories that are not convex, as the integers, or finite types, which the internal splits cover here.

#### Steps

Simplification repeats these steps on a sequent until nothing changes:

1. **Evaluation and unfolding**: closed terms are evaluated by the same unfolding and the builtins on literals, without `run.u`, recursive functions are unfolded where their `match` or `if` is decided, `length([x; xs])` to `1 + length(xs)`, but not `length(xs)`, which terminates since functions do, functions that are not recursive are unfolded always, as abbreviations, and beta reduction.
2. **Normalization** of the connectives into the sequent structure, see above.
3. **Rewriting** to normal form with the rules of the context and the canonical forms of the state.
4. **Deciding** atoms by the state of CC(X), and closing and dropping, see above.
5. **Splitting**, internally, on `if` and `match` whose test or argument is not decided, on ground clauses, and on disjunctions that theories imply, kept only if all branches close.

**The term order**, e.g. a lexicographic path order whose precedence is the order of declarations, with later functions above earlier ones, makes rewriting terminate: rewrite rules must decrease in it, and facts are oriented by it. A limit of steps is a safety net, which leaves an equivalent goal that is not normalized. Unfolding interprets any function, so simplification is a procedure.

#### Modules

| Module | Contents |
|---|---|
| `simp/term.u` | `Term`, the signature, opening and closing, substitution, matching, the term order, and printing |
| `simp/seq.u` | `Seq`, normalization, and printing |
| `simp/cc.u` | the core of CC(X) and the interface of the theories |
| `simp/constr.u` | constructors |
| `simp/arith.u` | sums, solving, and Fourier–Motzkin |
| `simp/simp.u` | contextual simplification with `Ctx` |
| `self/prove.u` | the translations between `Expr` and `Term`, and the signature of a checked program |

`simp/` is tested on its own, with terms and sequents built by its tests, and the tests run with those of `tests/` and `self/`. `self/prove.u` imports modules of both directories, which needs imports across directories, see `TODO.md`.

Until the parsers read lemmas, goals are `Bool` functions, whose parameters are the fixed variables, with tests of goals that must close and of goals with their expected open form, and a command `simp <file> [<name> …]` that prints `name proved` or the simplified goal. An induction step is then a goal with the hypothesis as an assumption, e.g. `length(xs ++ ys) == length(xs) + length(ys) ==> length([x; xs] ++ ys) == length([x; xs]) + length(ys)`.

References: G. Nelson, D. Oppen, Simplification by Cooperating Decision Procedures, TOPLAS 1979; R. Shostak, Deciding Combinations of Theories, JACM 1984; H. Rueß, N. Shankar, Deconstructing Shostak, LICS 2001; S. Krstić, S. Conchon, Canonization for Disjoint Unions of Theories, Information and Computation 2005; S. Conchon, E. Contejean, J. Kanig, S. Lescuyer, CC(X): Semantic Combination of Congruence Closure with Solvable Theories, ENTCS 2008; D. Miller, A Logic Programming Language with Lambda-Abstraction, Function Variables, and Simple Unification, 1991; W. Pugh, The Omega Test, 1991; A. Kovács, elaboration-zoo, for indices in terms and levels in contexts.

### Open points

1. **Proofs of programs**: the forms above inside the points `at …` of loops and calls, and which of the ways to refer to points of the code.
2. **Arithmetic**: whether Fourier–Motzkin with tightening suffices for inequalities, or the Omega test, which is complete for integer linear arithmetic, is needed.
3. **Orientation**: rewrite rules only from left to right, or also `using <- l`, and which term order, where the precedence by declarations may reject natural rules.
4. **More induction**: on `Int`, `induction n` for `n >= 0` with the cases `0` and `n + 1`, and computation induction `induction f(xs, ys)`, whose cases follow the `match` of `f`.
5. **`calc` chains** `e1 == e2 by p == e3 by p` for equational proofs, which `have` covers for now.
6. **Names**: `subgoals` or a shorter keyword, whether conjunctions give their parts as subgoals without restating them, and `rewrite lemma` or another marker.
7. **Partial instantiation**: `h(a, _)`, which leaves `y` universal, or by name, `h(y := b)`, for facts and lemmas alike.
8. **Facts kept from simplification**, e.g. `have h := e only`, used only when cited, if a fact rewrites goals into an unwanted form, left out until needed.
9. **Unfolding functions that are not recursive** always may break abstractions, e.g. `max`, which could be kept by a marker.
10. **Refs in claims**: a ref of the procedure would be a parameter of the lemma for its initial value and an out for its final value, e.g. `lemma inc_correct(x: Int): (y: Int) := correct inc(ref x => y)`, which needs a syntax for the pair, and then needs no `old` in the lemma.
11. **Contracts without a `correct` lemma**: tried by a default proof, or reported as unproven.
12. **Several contracts per procedure**, e.g. as claims `correct p ensures …`, which calls would all assume.
13. **Total correctness**, by `decreases` on loops and procedures, and claims about exceptions, e.g. `raises`.
14. **Scala** only parses specifications, lemmas, and proofs and drops them, since the tests of `parse.u` compare both parsers on `tests/`, while the verifier is in `simp/` and `self/`.
15. **Runtime checks** of contracts in the interpreter, which make specifications useful in tests before they are proven.
16. **Quantifiers and the functions `exists` and `forall`** of `list.u` share their names, so `forall x => e` in specifications would clash with `forall(xs, p)`, unless the quantifiers are keywords only in specifications, or the functions are renamed, e.g. to `any` and `all`, or the quantifiers as constants of one argument coexist with them by arity, which may not work across modules.
17. **Splitting on ground clauses** among the assumptions, or only unit propagation, where splits are a search, but finite and safe.
18. **Theories as values or tags**: a datatype of functions per theory, or a tag dispatched by `match`, which is simpler and faster for two theories.
19. **Equations between functions** among the assumptions, e.g. `f == lambda x => x + 1`, which need extensionality and congruence through `@`, left out at first.
20. **Speed**: union-find on maps costs a logarithm per operation, and terms compare structurally, where interning terms to numbers in the core is an option, while hash consing of all values in the runtime was deemed not worthwhile.
