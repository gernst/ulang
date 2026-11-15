# Or-patterns

A plan: the traversals that end in `case _` named by alternatives, `case A | B =>`, as Code 1 asks, now that the language has them (`CLAUDE.md`, Syntax and translations). Delete this file when the steps below are done, and move what stays true into `CLAUDE.md`.

The steps, each after the one before: `make selfcompile`, so that the seed parses `|`; the rule of style below in `CLAUDE.md`, Style, instead of that of the datatypes that grow; the traversals, one commit per package; then the lint and the refactoring.

## The traversals

The `case _` of the traversals become alternatives of the constructors they stand for, which Code 1 asks for, so that a new constructor is an error of exhaustiveness at each of them:

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

- The backends multiply nested alternatives out into copies of the case, which needs a limit, or a test of the alternatives where they stand that binds the variables of each, only if a program ever makes many.
