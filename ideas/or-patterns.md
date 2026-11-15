# Or-patterns

A plan: alternatives in patterns, `case A | B => e`, and then the traversals that end in `case _` named by them, as Code 1 asks. Delete this file when both are done, and move what stays true into `CLAUDE.md` and `README.md`.

## Why

- The rule of style for matches over `Cmd`, `Expr`, and `Stmt` names every constructor, so that the exhaustiveness check lists each place to change, but today that copies the body for each constructor, which is why the traversals of Code 1 keep their `case _`, and why a new constructor slips through them silently. With alternatives, the constructors stay named and the body is written once, `case PWild | PLit(_) => pat`.
- `|` is what agents guess, from OCaml, Rust, F#, Scala, and Python. Stacked heads, `case A => case B => e`, were rejected: a forgotten body, today a parse error at the next `case`, would mean fall-through; they cannot nest; and guards, layout, and the check of variables would need rules for a run of heads.
- `|` is free: it is no keyword and no character of operators (`is_op_char` of `ulang/syntax/syntax.u`); the lexer only rejects it, with hints for `||` and `|` (`character_hint` of `ulang/parse/lex.u`).

## The language

- `p | q` is a pattern, anywhere a pattern stands, also nested, `Some(A | B)`, and in `catch` and `assert raises E | F do … end`, there of one type of exception at first (Open).
- `|` binds looser than the arguments of constructors and tuples and tighter than `: T`, so `case A | B: T =>` annotates `B`; since all alternatives match the same value, the annotation fixes the type of all of them.
- Every alternative binds the same variables with the same types, else an error at the alternative, naming the variable that it lacks or binds with another type and the alternative that binds it, as `AliasVariables` does for pattern aliases (`OrVariables` in `ulang/check/check.u`).
- A guard applies to the case, all alternatives: `case A(x) | B(x) if x > 0 =>`.
- An alternative that the cases before cover, or the alternatives before it, is `Unreachable` at that alternative, not at the case.
- A pattern alias whose pattern has alternatives only matches, as one with `_` does (`PatternOnly`), since it does not say which alternative to build; the rule of style that aliases also build keeps such aliases out of the sources.
- Irrefutable places, `let`, `var`, lambdas, generators, and `for`, take alternatives as any pattern, checked for exhaustiveness as now.
- Splits of proofs, `match` and `induction`, take them as the cases of checked code do, below; if that needs more than the expansion, they are a parse error with a hint at first.

## The translation

The checked program keeps the alternatives as `POr`, one case with one guard and one body, so that the checker, the raises, and the hints see the case as written, and only the phases that run or reason case by case take it apart:

- The checker: `binders` types each alternative against the type matched and checks that they bind the same variables with the same types; the guard and the body are checked once. Exhaustiveness (`missing` and `is_useful` of `ulang/check/check.u`) splits a row whose first column is `POr` into one row per alternative, as the matrices of Maranget do, so that an alternative is `Unreachable` if the rows before it, and the alternatives before it, cover it. The raises are unchanged, since the body is one.
- The interpreter tries the alternatives from left to right, and binds the variables of the first that matches (`match_into` of `ulang/run/run.u`).
- The compilers test the alternatives one after another, each binding the variables from its own position in the value, and run the one body after the first that matches, without copying it: in C an `if` per alternative that binds and sets a flag, then the guard and the body under it (`pattern_tests` of `ulang/compile/compile.u`), in assembly a label of the body that each alternative jumps to after binding (`test_pat` of `ulang/compile/x86.u`). Nested alternatives are tested the same way, by a disjunction where they stand, so nothing is multiplied out. What both backends share, the alternatives with the paths of the variables in each, goes into `ulang/compile/lower.u`.
- The prover splits the rows of a `POr` into one per alternative in `leaves` of `ulang/prove/patterns.u`, which already makes a leaf per case and value, so the equations of definitions, the termination analysis, and the splits of proofs get one leaf per alternative with the same body.
- Pattern aliases keep their `POr` when `subst_pat` of `ulang/check/check.u` expands a use.

## The parts to change

1. `ulang/syntax/syntax.u`: the constructor `POr(alts: List[At[Pat]])` in `Pat`, as written, which `strip_pat`, `pattern_names`, and `pattern_vars` of `ulang/syntax/syntax.u` walk.
2. `ulang/parse/lex.u`: `|` a token of its own, never part of an operator; the hints move from `character_hint` of `ulang/parse/lex.u` into the parser, `hint_at` and `operator_hint` of `ulang/parse/parse.u`: `|` and `||` between expressions get `; | separates the alternatives of a pattern; the operators of Bool are and, or, and not`.
3. `ulang/parse/parse.u`: `parse_pattern` of `ulang/parse/parse.u` reads alternatives, with recovery at the next `|`, `=>`, `,`, or `)`.
4. `ulang/format/format.u`: `format_pattern` of `ulang/format/format.u` and `format_pat` of `ulang/format/format.u` print `A | B`; a head wider than 80 characters has one alternative per line, each after `| ` one level deeper than `case`, with the comments between them placed as in other sequences of lines; `case_chain` of `ulang/format/format.u` counts the head as one.
5. `ulang/check/check.u`: the typing and the rows of exhaustiveness above, and `OrVariables` with its message in `located_message` of `ulang/check/message.u`, with the hint for the habit of writing a variable in one alternative only.
6. `ulang/lint/lint.u`: `placed_pat_vars`, `part_pat_names`, and `binder_lints` of `ulang/lint/lint.u` walk the alternatives of the parsed module; a variable is unread if the body reads it in none, reported once, at the first alternative.
7. Every match over `Pat` handles `POr` itself, since the checked program has it: the interpreter, both backends, and the prover as above, and the walks, such as the variables and names of a pattern, by its first alternative, whose variables are those of all. The 23 matches over `Pat` with a `case _` in `ulang/` (below) are read by hand in this step, since a `case _` takes `POr` silently, as `zonk_pattern` of `ulang/check/check.u` would leave the types of its alternatives unsolved: also `subst_pat` and `head_of` of `ulang/check/check.u`, `pattern_tests` of `ulang/compile/compile.u`, `test_pat` of `ulang/compile/x86.u`, `pattern_vars` of `ulang/prove/syntax.u`, and the others that the scan lists.
8. Tests: matches of expressions and statements, nesting, guards, `catch` of two exception types, and `assert raises`, in `ulang/test/datatypes/datatypes.u` and `ulang/test/statements/statements.u`, also as the round trip of `format`; cases of `ulang/test/errors/`: `OrVariables` and `Unreachable` of an alternative in `types.u` and `matches.u`, and the moved hint of `||`, the case `operator-or.u` in `ulang/test/errors/parse.u`, whose expected exceptions come from a run, by `make testerrors`, after asking.
9. Docs: `README.md`, Language; in `CLAUDE.md`, Syntax and translations, where the alternatives are taken apart, and Style, the rule below, which replaces that of the datatypes that grow.

Steps 1 to 9 are one commit, which teaches the seed; then `make selfcompile`, and only then may the sources use `|`.

## The traversals

After the seed knows `|`, the `case _` of the traversals become alternatives of the constructors they stand for, which Code 1 asks for, so that a new constructor is an error of exhaustiveness at each of them:

- The rule, for every datatype alike, with no list of those that grow: a match names the constructors that a new one would need a decision beside, with `|` where they share a body. What decides is the role of the match, not its datatype: a predicate or selector of one constructor keeps its `case _`, where every other constructor means the same, and so does a match where another place decides, as `enter_section` of `ulang/parse/parse.u` does, and a dispatch on the next token whose `case _` is the parse error of everything else.
- A constructor that an earlier phase rules out gets a case of its own that raises `Unexpected`, with the doc of that phase, in procedures; in functions, which cannot raise, the alternative of the constructors ruled out stays apart from the others, with the doc, as `private_in` of `ulang/check/check.u` has it today for its `case _`.
- The scan of 15 November 2026, by the constructor names in the heads of the cases, outside the directories `test/`, found 403 matches with a `case _` over the datatypes of `ulang/`: 280 that name one constructor, mostly selectors and predicates, which stay; 46 that name two, and 77 that name three or more, to be classified one by one, most by far over `Term`, `Expr`, `Cmd`, `Pat`, `Token`, `Type`, `TypeError`, and `Val`. Of the syntax alone, `Cmd`, `Expr`, `Stmt`, `Pat`, and `Type` of `ulang/syntax/syntax.u` and `Term` of `ulang/prove/syntax.u`, 46 name three or more, nearly all traversals:
  - `ulang/check/check.u`: `has_holes`, `subst`, `occurs`, `solved_type`, `zonk_pattern`, `zonk_cmd`, `unknown_in_expr`, `unknown_in_cmd`, `head_of`, `has_error_type`, `subst_pat`, `command`, `native`, `private_in`, `exposed`, `example_owner`, and `spec_of`
  - `ulang/prove/syntax.u`: `rebind`, `pattern_vars`, `subst`, `replace`, `occurs`, `loose`, `head_key`, `args_of`, and `print_arg`; `ulang/prove/simp.u`: `simp_term`, `candidate`, and `is_plain`; `ulang/prove/termination.u`: `calls_in`
  - `ulang/compile/compile.u`: `pattern_tests` and `compile_cmd`; `ulang/compile/x86.u`: `test_pat` and `compile_cmd`; `ulang/compile/lower.u`: `type_key`
  - `ulang/syntax/syntax.u`: `rejected`, `is_signature`, and `sorts_in`; `ulang/parse/parse.u`: `without_examples` and `step_of`
  - `ulang/lint/lint.u`: `is_branching`, `is_root`, `declared_name`, `cmd_names`, `placed_pat_vars`, and `part_pat_names`
- One commit per package, each checked by `ulang.sh check`, and `make test` once at the end, since the conversion keeps behaviour, except where naming the constructors finds one that the `case _` handled wrongly, which is a fix of its own, committed apart.
- Afterwards: the operation `expand-wildcard`, in `ideas/refactor.md`, writes one case with the alternatives instead of one case per constructor, and the lint of a `case _` (Diagnostics 5) proposes it where the match names two or more constructors besides it, for every datatype, so that `project.u` declares no list of datatypes that grow, which Code 1, Diagnostics 5, and `ideas/refactor.md` then no longer ask for.

## Open

- Handlers whose alternatives have different types of exceptions, `catch case OsError(m) | ParseError(m) =>`: a `Catch` has one type, which the interpreter and both compilers compare with the type of the exception before the pattern, and by which the raises cover a `try` (`handled` of `ulang/check/raises.u`). Either the alternatives of a handler have one type, the rule at first; or `Catch` carries a type per alternative, which every phase that reads `Catch.typ` must then follow; or the checker splits such a handler into one `Catch` per type, each with the alternatives of its type, the one place where the checked program would not keep a case as written.
- The leaves of the prover multiply out nested alternatives, as they do nested patterns now, which needs a limit only if a program ever makes many.
