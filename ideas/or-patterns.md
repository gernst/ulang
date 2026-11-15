# Or-patterns

A plan: the matches that end in `case _` named by alternatives, `case A | B =>`, as Code 1 and the rule of style in `CLAUDE.md`, Style, ask, now that the language has them (`CLAUDE.md`, Syntax and translations). Delete this file when the steps below are done, and move what stays true into `CLAUDE.md`.

Done: the 12 sites over `Type` of `ulang/syntax/syntax.u`, and `zonk_type` of `ulang/check/check.u`, whose catch-all is a variable, `raises_modules` of `ulang/check/uses.u` over `Raises`, and of `Expr`, outside the prover, `unknown_in_expr` of `ulang/check/check.u`, where the other matches over `Expr` with a catch-all take apart one or two shapes and keep it: `placed_lints` of `ulang/lint/lint.u`, `list_literal` and `consing` of `ulang/compile/lower.u`, `operand_of` of `ulang/compile/x86.u`, and `with_container` of `ulang/check/check.u`; and of `Val`, `format_value` and `incomparable` of `ulang/run/run.u`, where `apply_value`, `primitive`, `invoke_value`, and `os_proc` of `ulang/run/run.u` keep their catch-all, an error, `list_elements` of `ulang/run/run.u` is a selector, `equal` and `compare_values` of `ulang/run/run.u` match pairs, and `show_in` of `ulang/run/run.u` leaves the others to `format_value`. And of `Cmd`, by the aliases `CSig`, `CNoCode`, and `CNoSpec` of `ulang/syntax/syntax.u` where a site treats the group alike, 13 sites, where `command` of `ulang/check/check.u` now raises `Unexpected` for the signatures, abstract types, and pattern aliases that `placed_definitions` keeps from it, while `native`, `exposed`, and `is_signature` keep their catch-all, maps of three or four commands. The steps, each after the one before: the other sites below, one commit per package, `ulang/check` first, handed over to `main` after each package or two, since other sessions edit `ulang/check/check.u` all the time; then the lint and the refactoring.

## The sites

The scan of 15 November 2026 (`main` at 1ccb2d3), by the constructor names in the heads of the cases, outside the directories `test/`, found 109 matches with a `case _` that name two or more constructors of one datatype, and 3 whose catch-all is a variable, `case u =>`, of which `zonk_type` of `ulang/check/check.u` is a traversal and `field_access` of `ulang/check/check.u` and `at_stmt` of `ulang/parse/parse.u` keep theirs, a dispatch whose catch-all is an error and one on the next token; 5 of them are matches on tuples whose heads only name `Some` and `None` inside, which drop out. The others, by the rule of style:

- Convert (60 sites):
  - `has_holes`, `subst`, `occurs`, `solved_type`, `zonk_pattern`, `zonk_cmd`, `unknown_in_expr`, `unknown_in_cmd`, `head_of`, `constructors_of`, `has_procs`, `loosened`, `takes_fixed`, `has_error_type`, `command`, `native`, `private_in`, `pattern_constrs`, `exposed`, `example_owner`, and `spec_of` of `ulang/check/check.u`
  - `type_modules` and `raises_modules` of `ulang/check/uses.u`
  - `format_operand` of `ulang/compile/c.u`
  - `pattern_tests`, `pattern_binds`, and `compile_cmd` of `ulang/compile/compile.u`
  - `type_key` of `ulang/compile/lower.u`
  - `test_pat`, `bind_pat`, and `compile_cmd` of `ulang/compile/x86.u`
  - `infix_operand`, `prefix_operand`, and `postfix_operand` of `ulang/format/format.u`
  - `declared_name`, `cmd_names`, `placed_pat_vars`, and `part_pat_names` of `ulang/lint/lint.u`
  - `without_examples` of `ulang/parse/parse.u`
  - `shape` of `ulang/prove/egraph.u`
  - `walk` of `ulang/prove/equations.u`
  - `term_rank`, `simp_term`, `is_plain`, and `term_size` of `ulang/prove/simp.u`
  - `rebind`, `pattern_vars`, `subst`, `replace`, `occurs`, `loose`, `head_key`, `args_of`, and `print_arg` of `ulang/prove/syntax.u`
  - `calls_in` and `accesses_in` of `ulang/prove/termination.u`
  - `format_value` and `incomparable` of `ulang/run/run.u`
  - `rejected` and `sorts_in` of `ulang/syntax/syntax.u`
- Review, convert unless reading them shows that every other constructor means the same (7 sites):
  - `form_expr`, `with_container`, and `signature_of` of `ulang/check/check.u`
  - `is_root` of `ulang/lint/lint.u`
  - `step_of` of `ulang/parse/parse.u`
  - `candidate` of `ulang/prove/simp.u`
  - `is_signature` of `ulang/syntax/syntax.u`
- Keep (37 sites):
  - dispatches on the next token: `word_of` of `ulang/parse/lex.u`; `is_command_token`, `literal_at`, `operator_at`, `starts_type`, `starts_expr`, `starts_pattern`, `starts_stmt_after`, `ends_expr`, `parse_stmt`, and `at_operator` of `ulang/parse/parse.u`
  - matches over `TypeError`, whose 59 constructors would cost 48 to 56 lines each: `error_types`, `is_unify_error`, `is_blocking`, and `unparsed_use` of `ulang/check/check.u`; `type_details` and `hint_of` of `ulang/check/message.u`
  - predicates: `signs` of `ulang/check/check.u`; `is_skip`, `is_variable`, and `is_branching` of `ulang/lint/lint.u`; `is_variable` of `ulang/prove/patterns.u`; `is_catch_all` of `ulang/prove/proof.u`; `is_valid_rule` and `is_branching` of `ulang/prove/simp.u`; `is_lemma_or_test` of `ulang/syntax/syntax.u`
  - selectors: `declared_var` and `private_sort` of `ulang/check/check.u`; `matched_levels` of `ulang/prove/equations.u`; `proof_errors` of `ulang/prove/lemma.u`; `decided_body` of `ulang/prove/simp.u`; `constr_head` of `ulang/prove/syntax.u`
  - dispatches with a default: `simp_app` of `ulang/prove/simp.u`; `show_in` of `ulang/run/run.u`
  - dispatches whose `_` is an error: `apply_value` and `invoke_value` of `ulang/run/run.u`
  - rewrites of one shape: `placed_lints` of `ulang/lint/lint.u`

The conversion of a site replaces its `case _` by the alternatives of the constructors that it stands for, with `_` for their fields, so that a new constructor is an error of exhaustiveness at each site:

- A constructor that an earlier phase rules out gets a case of its own, as the rule of style says, where the doc of the `case _` names it, as at `unknown_in_expr`, `private_in`, and `exposed` of `ulang/check/check.u`.
- Each site checked by `out/ulang.sh check ulang/ulang.u`, since the conversion keeps behaviour, and `make test` at the end, with `make testc` and `make testelf` after asking, since the compiler then compiles its own matches by the code of alternatives; except where naming the constructors finds one that the `case _` handled wrongly, which is a fix of its own, with a test, committed apart. `step_of` of `ulang/parse/parse.u`, whose `case _` raised the message of `assert raises` for every other statement, was no bug, since `parse_proof` of `ulang/parse/parse.u` parses a statement only at `assert` or at a name, and now names `SAssertRaises`; `form_expr` of `ulang/check/check.u` names `FList` and `FSet`.

## The cost

- Source: 152 lines more for the 67 sites of Convert and Review, 0.4 % of the 33,981 lines of `ulang/`, since 42 of the heads stay on one line and the others spread one alternative per line, the format of a head wider than 80 characters; the sites of Keep would cost 434, by far most of them for `TypeError` and the tokens.
- Compiled: the 67 sites have 329 alternatives, 4.9 per site, and the C of a case with alternatives is an `if` with a flag per alternative, about 3.6 lines each and 4 per case (`case_stmts` of `ulang/compile/compile.u`), so about 1,450 lines more of the 108,803 of `out/ulang.c`, 1.3 %, and a test of the constructor per alternative where `case _` tested nothing. A case whose alternatives bind no variable and have no guard, as nearly all of these, needs no flag: one `if` of the disjunction of their tests, so about one line per alternative, and the last case of a match whose alternatives cover every constructor that the cases before leave, which exhaustiveness knows, needs no test at all, so that the conversion costs nothing in the compiled code (Compiler item, to file when the sites are done).

## Pattern aliases with alternatives

A pattern alias may have alternatives on the right, `pattern TLeaf(): Type := THole | TMeta(_) | TParam(_)`, which checks and matches today, `case TLeaf =>`, and builds no value (`PatternOnly`). It would name a set of constructors that several sites share, but it helps little here:

- The sets that repeat: the leaves of `Type`, `TError`, `THole`, `TMeta`, and `TParam`, at 7 sites; and for `Cmd`, `Pat`, `Term`, `Shape`, and `Val`, sets of 2 or 3 sites each, 34 sites in 12 sets, of which only the three sets of `Cmd`, 9 sites, have heads that spread, 6 or 7 lines each; aliases of all 12 would save about 2 lines in all, since each costs its doc, its head, and its pattern.
- It moves the decision from the site to the alias: a new constructor added to `TLeaf` reaches all 7 sites without an error at any, which is what `case _` did, while the point of the conversion is that each site decides. So an alias fits only a set that is a concept of its own, whose members every site treats alike by definition, such as the commands without bodies.
- It conflicts with the rule of style that a pattern alias builds as well as matches.

So the sites name their constructors, and no aliases for now; an alias of a concept, such as the commands without bodies of `Cmd`, is a later choice where its sites agree.

## Afterwards

- The operation `expand-wildcard`, in `ideas/refactor.md`, writes one case with the alternatives instead of one case per constructor, and the lint of a `case _` (Diagnostics 5) proposes it where the match names two or more constructors besides it, for every datatype, so that `project.u` declares no list of datatypes that grow, which Code 1, Diagnostics 5, and `ideas/refactor.md` then no longer ask for; the sites of Keep need the line `// lint allows …` of Diagnostics 5, which the lint therefore waits for.

## Open

- The compilers multiply nested alternatives out into the tests, bindings, and guards of each combination, while the body is compiled once; a test of the alternatives where they stand, which binds the variables of each, only if a program ever makes many.
