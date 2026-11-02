# TODO

## Self-hosting

The parts in `self/` form a compiler from ulang to C written in ulang, which compiles itself.

- `lex.u` and `parse.u` parse as `Parser.scala` does, which their tests check against `read` and `library` on every file of `tests/`, `self/`, and the library.
- `check.u` with `ctx.u` checks as `Check.scala` does, and `run.u` interprets as `Run.scala` does.
- `c.u` and `compile.u` translate checked programs to C, which runs with `self/runtime` (GMP, the Boehm collector, and `setjmp` for exceptions).
- `ulang.u` is the driver: `./ulang.sh self/ulang.u -- source.u target.c` from the Scala interpreter, or `ulang source.u target.c` once compiled, with paths relative to `self/` for the interpreter.
- The compiled compiler produces the same C as the interpreted one on the tests, in a fraction of a second instead of minutes.

### Open points

1. **The bootstrap reaches a fixed point**: the interpreter compiles `ulang.u` to `ulang1.c` in about 8 minutes, whose program compiles `ulang.u` to `ulang2.c` in about 35 seconds, and both are identical. This has no make target or test yet, and 35 seconds for 400 KB of C suggests quadratic list operations, e.g. `++` on growing lists or association lists as maps.
2. **Builtins are registered in five places**: their types in `Check.scala` and `self/check.u`, and their implementations in `Run.scala`, `self/run.u`, and the runtime with `builtin_functions` of `self/compile.u`. Signatures declared in library modules, e.g. `builtin function str_get(s: Str, i: Int): Char`, would leave only the implementations, and would make a builtin exist once its module is imported.
3. **Builtins are enabled by datatype names**: `read` and `library` by any datatype `Program`, the `os_` procedures by `OsError`. Enabling them by the import of their module would be sturdier, e.g. with `syntax.u` in the library.
4. **Prefixes against clashes.** Hooking up the parser hit clashes of `literal`, `resolve`, `formals`, and `expr_cases`, which prefixes avoid. With modules, helpers of modules that are not imported together may share names, so the prefixes in `lex.u` and `parse.u` could go where no module imports both.
5. **No positions in `self/syntax.u`.** The parser knows offsets, but `check.u` and `compile.u` cannot say where an error is. A position per command would be a start.
6. **Errors of the driver.** `LexError`, `ParseError`, `TypeError`, `RuntimeError`, `CompileError`, and `OsError` leave `ulang.u` as uncaught exceptions, instead of a message with a position.
7. **The driver prints `args`**, since a run shows its local variables.
8. **Imports are resolved against the directory of the parsed file**, not of the importing module, in both parsers, and loaded modules are known by name. Resolving against the importing file, with modules known by their location, would make nested imports and library modules consistent, and make the base name of the parsed file in the loaded modules unnecessary.
9. **Paths read at run time** are relative to the program in the interpreter, but to the working directory in compiled programs.
10. **The file lists in the tests of `parse.u`** are written out, since there is no `os_list`.
11. **`read`, `library`, and `Quote`** only remain as the oracle of the tests of `parse.u`, and as stubs in the runtime, since imported procedures that call them are compiled.
12. **Parsing is slow in the interpreter**: compiling the compiler with it takes about ten minutes.

## Modules

A module is a file, and modules see only what they import directly. This is done in Scala and in `self/`:

- **Visibility.** A module sees its own declarations, the declarations of the modules it imports directly, and the prelude with `nat`, `option`, and `list`, where its own declarations hide imported ones, and a name that two imports declare is ambiguous where it is used. Imports are not transitive for notations either: a module is parsed with the notations of the prelude and of its direct imports. Cyclic imports are errors, and source files never use qualified names.
- **Syntax.** The first import of a module comes with `Module(name, cmds)`, and every import is the marker `Import(name)`.
- **Compound names** `Name(module, base)`: the parser leaves the module open, and the checker gives declarations their module and resolves every reference to a global, so that the checked syntax has qualified names, while locals and builtins have none. The main file is the module of its base name. Printers and messages show the base name.
- **Names created by hand**, by builtins and `Quote`, are defined once: `Names` in Scala, `list_name()` and the like in `self/syntax.u`.
- **The runtime** represents constructors by static descriptors `struct u_name`, compared by address, and C names contain their module, e.g. `f_functions_46_fact_1`.

### Open points

1. **Once per module.** A module depends only on the prelude and its direct imports, so it could be parsed, checked, and turned into interpreter tables once, which would spare the prelude for every file.
2. **Errors in modules have no file**: a type error in an imported module shows a position without its module.
3. **Imports are resolved against the directory of the parsed file**, not of the importing module, and modules are known by name instead of their location.
4. **Exports.** All declarations are exported, so helpers of different modules only coexist if no module imports both.

## The read procedure

1. **Any datatype named `Program` enables `read`**, not only the one of `self/syntax.u`. A program with its own `data Program := Foo(Int)` type-checks `call read(in path, out p)` and then fails at runtime, since `p` is a `Program(…)` from `Quote`.
2. **Only missing files are `OsError`s.** Other I/O failures, e.g. permissions or a path that `os.Path` rejects, throw Java exceptions that `Main` and `FileTest` do not catch.
3. **Parse errors in the read file** are reported against the reading program, without the read file or the call site, and cannot be caught by `try`.
4. **Builtin procedures are special-cased by name** in `Check.procedure` and `Run.invoke`, see the builtins above.
5. **Wrong arguments to `read`** fall through to the message "unknown procedure read".

## Compiler

1. **Variables after a raise.** `try` uses `setjmp`, after which C leaves local variables that changed since then undefined unless they are `volatile`. Variables whose address is taken, as for outs and refs, work in practice, but this is not guaranteed.
2. **Compiled programs are compared with the interpreter by hand**, while the test only writes their translations. The comparison could become part of the tests.
3. **Runtime errors have no positions**, and an unassigned variable is a `NULL` pointer instead of an error.
4. **Deep recursion.** Functions like `concat` and `str_join` over long lists recurse deeply, and compiled programs need a large stack (`ulimit -s unlimited`) for big inputs, as the interpreter runs programs on a thread with a large stack.

## Syntax

Points of a critique of the syntax, ordered by how much they cost when writing `self/check.u`, `self/run.u`, and `self/parse.u`.

1. **Records.** Datatypes have only positional fields, so `Ctx` in `self/ctx.u` needs an accessor and an update function per component, each matching `Ctx(a, f, p, d, m, k)`. Named fields would replace them: `c.funs`, `c with funs := …`, `Ctx { funs := f }`.
2. **Locals.** Every result of a procedure call needs a local, declared with a type at the beginning of a block. Inferring the type from an initial value would help: `local x := e`.
3. **No parsers as values.** Procedures are not values, and functions cannot fail, so `self/parse.u` repeats each loop of `repsep` and `rep1sep` per kind of item, and has the operator loop three times and `cases` twice, which makes it more than twice as long as `Parser.scala`.
4. **Patterns are weaker than expressions.** There are no or-patterns `case TParam(_) | TMeta(_) =>`, or as-patterns, which forced `self/run.u` to rebuild values it had just taken apart.
5. **One global namespace.** Functions, procedures, and constructors share one namespace, and overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Qualified names for modules (`check.infer`) or constructors (`Expr.Var`) would help.
6. **Smaller points.**
   - `assign` is redundant before `x := e`.
   - `[…]` is both type arguments and list literals.
   - `else` followed by `if` always continues a chain of statements, so a nested `if` cannot directly follow `else`.

Records, then `local` with an initial value, would shorten `self/check.u` the most.

## Design notes

What a language would ideally offer for writing code with an AI assistant, which types cheaply but makes mistakes it cannot see: implicit conversions, shadowed names, hidden effects, forgotten cases.

1. **Everything that matters is visible where it happens**: effects and data flow in signatures, no implicit conversions or overloading by type, and failure in the type, with short propagation like Rust's `?`.
2. **A strict, fast, and precise checker**: inference within functions, types at module boundaries, exhaustive matches, and errors with positions and expected types, since the checker is the feedback loop.
3. **Small and uniform**: few concepts, one way to write each, no context-dependent syntax, and a canonical formatter.
4. **Modules with explicit exports, and records with named fields**, since names are how code is navigated, and clashes and positions are where mistakes happen.
5. **Abstraction over effectful code**, such that patterns like `repsep` are written once.
6. **Specifications in the language**: pre- and postconditions and properties, checked by tests or a prover, since the common mistakes are subtle logic errors rather than type errors.

### Compared to ulang

- **Already there**: explicit data flow with `in`, `out`, and `ref`, pure expressions with effects only in statements, exhaustive matches and inference, a small language without implicit behavior, exceptions as values of a type, and named tests with `assert` showing both sides.
- **Missing**: records, modules with exports, procedures as values or functions that can fail, `local x := e`, positions in errors, and contracts. These are extensions, not redesigns, see the points above.
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

- **Outs of lemmas.** A lemma with a contract states that for all parameters that satisfy the `requires` there are outs that satisfy the `ensures`, so that its proof gives witnesses, which a proof as a program assigns, e.g. `assign q := m / n, r := m % n`, and a proof that uses the lemma obtains them with names of its own, e.g. `obtain (q, r) := division(a, b)`, a syntax still to be designed. In a claim `correct p(…)` the outs of the lemma are those of the call, which the run determines, not the proof: under partial correctness the claim holds for the outs of every run that ends normally, and under total correctness this is exactly the existential, since a run exists and gives them.
- **Scopes are explicit.** A proof sees only names that it or its lemma binds, never the parameters of the procedure implicitly:
  - the parameters and outs of the lemma, which name every argument of the call in the claim, `correct p(in n, out s)`,
  - at a point `at …` of the body, its locals in scope there, which the quoted code names, e.g. `i` in `at while i < n`.

  The names of the lemma may differ from the parameters of the procedure, whose contract is renamed to them, so that a proof does not change when a parameter is renamed.
- **No invariants in the code.** Loops are specified in the proofs of the procedures, see below.

### Meaning

1. **`correct p`** states for all ins and initial refs: if the `requires` hold, every run of the body that ends normally satisfies the `ensures`. The claim does not restate the contract.
2. **Calls use contracts**: at `call q(...)`, the `requires` of `q` are proven, the outs and refs havocked, and the `ensures` assumed. Callers depend only on the contract, not on whether or how it is proven.
3. **Recursion.** Each body is proven assuming all contracts, including its own and those of procedures that call it back, which is sound for partial correctness for all of them at once by induction on the depth of calls (Hoare's recursion rule, generalized to mutual recursion). Proofs have no order and no cycles.
4. **Unproven contracts** make this conditional: if the contract of `q` is not proven, the proofs that use it hold only under it, which `--verify` reports, e.g. `sum proved, assuming q`.
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

### The proof language

An open design point. It has to answer:

1. case analysis on values, on branches of the code, and on obligations,
2. induction, by schemes of datatypes or by recursive uses of the lemma,
3. using lemmas, instantiating, and rewriting with them,
4. intermediate facts and calculations, `have` and chains `a == b == c`,
5. points of the code, see above,
6. where automation ends, i.e. what the solver does unasked,
7. whether proofs are readable without the goals,
8. how much of a proof survives changes of the code and the lemmas.

The styles:

- **Tactic scripts** (Coq, Lean, Isabelle `apply`): `induction xs; simp; auto`, short and powerful, but unreadable without the goals, which the editor would have to show, and fragile.
- **Structured proofs** (Isar, Lean `calc`): `case Cons(y, ys) => have … show …`, readable and robust with local errors, but verbose and a larger language.
- **Hints only** (Dafny, Why3): invariants and `using` lemmas, small, with nothing to fall back on when the solver fails.
- **Proofs as programs** (lemma bodies of Dafny, VeriFast): the proof is a statement, `match` for cases, `assert` for facts, and a recursive call of the lemma for the induction hypothesis, sound by a termination check like that of functions, with the parameters of the lemma as ins and its outs as outs. A proof of `correct p` would be a sequence of `at …` with invariants, loop contracts, and facts at calls, each with such a statement where needed. This adds only `at` and `invariant`/`loop` to the statements, but gives up rewriting tactics like `simp`, which could return as builtin procedures or hints `auto [lemmas]`.

### Backend

Tactics or proof statements reduce obligations to goals (fixed variables, hypotheses, a conclusion), which an SMT solver closes: Z3 or cvc5 by SMT-LIB 2.6, datatypes as `declare-datatypes`, functions as `define-fun-rec` guarded by their `requires`, lemmas as axioms, polymorphism monomorphized, and lambdas defunctionalized or rejected in specifications.

### Open points

1. **The proof language**, see above.
2. **Refs in claims**: a ref of the procedure would be a parameter of the lemma for its initial value and an out for its final value, e.g. `lemma inc_correct(x: Int): (y: Int) := correct inc(ref x => y)`, which needs a syntax for the pair, and then needs no `old` in the lemma.
3. **Contracts without a `correct` lemma**: tried by a default proof, or reported as unproven.
4. **Several contracts per procedure**, e.g. as claims `correct p ensures …`, which calls would all assume.
5. **Total correctness**, by `decreases` on loops and procedures, and claims about exceptions, e.g. `raises`.
6. **`self/`** at first only parses specifications, lemmas, and proofs and drops them, since the tests of `parse.u` compare both parsers on `tests/`.
7. **Runtime checks** of contracts in the interpreter, which make specifications useful in tests before they are proven.

## Review

Open findings of a review of the named fields, where the others are resolved: `self/` runs in Scala again, `C()` parses, and repeated fields and fields of different types are errors at the declaration.

1. **Accessors need known types.** `e.f` needs the type of `e` when the access is inferred, so a lambda `lambda p => p.x` fails with an unknown type, although the type would be inferred later. Resolving accesses at the end of the command, with a fresh type for the result in the meantime, would remove this restriction.
2. **Accesses as variables.** An access is the application of a variable `.f` until the checker resolves it, instead of a node `EField(e, f)` of the syntax. Only the parser produces it, always applied, so other passes do not see it, but a node would be clearer, which goes well with resolving accesses later.
3. **Accessors ignore visibility.** `e.f` resolves to the accessor of the module of the type of `e`, also if the module of the access does not import it, which is intended: whoever has a value of a type may read its fields.
