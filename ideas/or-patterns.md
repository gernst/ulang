# Or-patterns

A plan: the matches that end in `case _` named by alternatives, `case A | B =>`, as Code 1 and the rule of style in `CLAUDE.md`, Style, ask. Delete this file when the steps below are done, and move what stays true into `CLAUDE.md`.

Done, outside the prover: the sites over `Type`, `Raises`, `Expr`, `Val`, `Cmd` (by the aliases `CSig`, `CNoCode`, and `CNoSpec` of `ulang/syntax/syntax.u`), `Shape`, and `Pat`. The steps, each after the one before: the sites below, one commit per package, handed over to `main` after each package or two, since other sessions edit the same files; then the lint and the refactoring.

## The sites

Left of the scan of 15 November 2026 (`main` at 1ccb2d3), by the `case _` that remain on `main` at 2e7674a:

- Convert (18 sites):
  - `format_operand` of `ulang/compile/c.u`
  - `shape` of `ulang/prove/egraph.u`
  - `walk` of `ulang/prove/equations.u`
  - `term_rank`, `simp_term`, `is_plain`, and `term_size` of `ulang/prove/simp.u`
  - `rebind`, `pattern_vars`, `subst`, `replace`, `occurs`, `loose`, `head_key`, `args_of`, and `print_arg` of `ulang/prove/syntax.u`
  - `calls_in` and `accesses_in` of `ulang/prove/termination.u`
- Review, convert unless reading them shows that every other constructor means the same: `signature_of` of `ulang/check/check.u`, and `candidate` of `ulang/prove/simp.u`.
- Keep, by the rule of style: the dispatches on the next token of `ulang/parse/lex.u` and `ulang/parse/parse.u`; the matches over the 59 constructors of `TypeError`; predicates and selectors of one or two constructors; dispatches with a default, or whose `_` is an error, as `apply_value` and `invoke_value` of `ulang/run/run.u`; rewrites of one shape, as `placed_lints` of `ulang/lint/lint.u`; and the maps of three or four constructors that the converted packages kept, `native`, `exposed`, and `with_container` of `ulang/check/check.u`, `is_signature` of `ulang/syntax/syntax.u`, `step_of` of `ulang/parse/parse.u`, and `is_variable` of `ulang/lint/lint.u`.

The conversion of a site replaces its `case _` by the alternatives of the constructors that it stands for, with `_` for their fields, so that a new constructor is an error of exhaustiveness at each site:

- A constructor that an earlier phase rules out gets a case of its own, as the rule of style says, where the doc of the `case _` names it, as at `unknown_in_expr`, `private_in`, and `exposed` of `ulang/check/check.u`.
- Each site checked by `out/ulang.sh check ulang/ulang.u`, since the conversion keeps behaviour, and `make test` at the end, with `make testc` and `make testelf` after asking, since the compiler then compiles its own matches by the code of alternatives; except where naming the constructors finds one that the `case _` handled wrongly, which is a fix of its own, with a test, committed apart.
- A pattern alias of alternatives names a set of constructors only where the set is a concept of its own, which every site treats alike by definition, as the commands without bodies of `CNoCode`, since a constructor added to an alias reaches all its sites without an error at any, as `case _` did.

## The cost

The C of a case with alternatives is an `if` with a flag per alternative (`case_stmts` of `ulang/compile/compile.u`), about 1,450 lines more of `out/ulang.c` for all sites, 1.3 %, and a test of the constructor per alternative where `case _` tested nothing. A case whose alternatives bind no variable and have no guard, as nearly all of these, needs no flag, one `if` of the disjunction of their tests, and the last case of a match whose alternatives cover every constructor that the cases before leave, which exhaustiveness knows, needs no test at all, so that the conversion costs nothing in the compiled code (a Compiler item, to file when the sites are done).

## Afterwards

The operation `expand-wildcard` of `ideas/refactor.md` writes one case with the alternatives, and the lint of a `case _` (Diagnostics 5) proposes it where the match names two or more constructors besides it, for every datatype, so that `project.u` declares no list of datatypes that grow; the sites of Keep need the line `// lint allows …` of Diagnostics 5, which the lint therefore waits for.
