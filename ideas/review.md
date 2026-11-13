# Review of smells and cleanups

A read-only review of `ulang/`, `checks/`, and the Makefile against the rules of `CLAUDE.md`, Style, and the open items of `TODO.md`, now `ideas/`, as of c319a1d. No source was changed. Findings that are instances of an item of `TODO.md` are tagged with it, e.g. **[Code 1]**; untagged findings are new. Line numbers are those of c319a1d, and the parts of the compiler that `ulang/compile/lower.u` took since are there now. Checked again against main on 2026-11-13: the findings that were fixed since are gone, and those below still hold, but for those marked as fixed since; the new items that section 4 proposes are Verification 9 and Compiler 5 to 8.

Confidence: **confirmed** means reproduced by a run, with the repro given; **likely** means read from the code and checked against it, not run.

## 1. Bugs

### 1.1 Confirmed

1. Fixed since, by the lexicographic order of inductions of `ulang/prove/proof.u`.
2. Fixed since, by 6c1aced.
3. **Sets and maps take any result of `compare` other than -1 and 1 as equal** (`ulang/prelude/set.u:59-105`, `ulang/prelude/map.u:60-204`, `case -1`, `case 1`, `case _`), while both backends pass the result of a declared `compare` hook through unchanged (`compare_in` of `ulang/run/run.u:842-846`, `compare` of `ulang/runtime/ulang.c:397`). With `function compare(a: V, b: V): Int := a.n - b.n`, `contains(V(1), Set(V(5)))` is true and `put`/`remove` hit the wrong entry. `CLAUDE.md` promises -1, 0, 1, but nothing enforces it. Fix: normalize the hook's result by its sign in both backends, with a case in `ulang/test/datatypes/orders.u`.
4. Fixed since, by b4dc673.
5. Fixed since, by 936711b.

### 1.2 Likely

Backends and runtime:

6. **`==` differs between the backends inside values of an abstract type that declares `==` but no `compare`** (`equals_ref` of `ulang/compile/compile.u:287-292`, `u_equals_by_compare` and `compare_items` of `ulang/runtime/ulang.c:405, 439-461`, `equal_all` of `ulang/run/run.u:776-795`). Compiled, a tuple, list, or datatype without its own `==` equates by `compare` of its fields, which raises `Unordered` at such a field, e.g. `Tally` of `ulang/test/modules/exported.u`; interpreted, it equates by `==` of the fields and succeeds. Fix: a structural `u_con_equals` by `u_equals`, with a case of a `Tally` in a `List`.
7. **The binary of the tests exits 1 for failed tests** (`u_run_tests`, `ulang/runtime/ulang.c:1318`), where `ulang.sh test` exits 3, and `CLAUDE.md` says `make testc` has the outcome of `make test`.
8. **Fused comprehensions evaluate in another order than `list.foldr`** (`ulang/compile/compile.u:1104-1118`): the elements first to last, then the initial value, against `init` first, then last to first. It shows only when two parts raise different builtin exceptions, but it is a divergence of the backends.
9. Fixed since, by b4dc673.
10. **NUL in paths and `os_write("")`** (`ulang/runtime/ulang.c:240-244, 1098-1158`): a path with a code point 0 is cut at it, so `os_read("a\0b")` opens `a` instead of raising `OsError`; `p_os_write` of an empty path reads `dir[1]` of a one-byte buffer.
11. **`format_str` writes control characters raw into C literals** (`ulang/compile/c.u:124-127`), only the escapes of ulang are applied; `chr(27)` or `chr(0)` in a literal reach the C source as is. Fix: octal escapes below 32 and for 127.

Checker:

12. Fixed since, by 474e902: a wrong arity of an overloaded name gives only `WrongArity`.
13. Fixed since, by bce0755.
14. Fixed since, by 6e845c5.
15. Fixed since, by 26716fe.
16. **Trials of candidates count errors instead of testing unification** (`trials` and `fits`, `ulang/check/check.u:1570, 2379`), while `report` drops any `CannotUnify` that reaches `TError`, so every candidate fits an argument of an erroneous type. Mostly masked by `settle`; low confidence of a visible effect.

Parser and driver:

17. Fixed since, by 2a083fb.
18. Fixed since, by 3beba00.
19. Fixed since, by b4dc673.
20. Fixed since, by 5442849.
21. **`load_texts` of `ulang/test/cases/cases.u:172-179` requires only the main text formatted**, so the members of a case of several modules may be unformatted, unlike `check`, which requires it of every member of the unit.

Prover, latent:

22. **Congruence over abstract types** (`merge` of `ulang/prove/simp.u:618-652`): the e-graph treats the declared `==` as identity, so observers, matches on the representation, and `Size` follow by congruence; a non-structural `==` could prove a false lemma or termination inside its module.
23. **Recursion through a function-valued field is not seen** (`ulang/prove/translate.u:105-117`), so such a function unfolds without a proof of termination, bounded only by the 256 unfoldings.
24. **Fallbacks that build fake terms** where an `Unexpected` belongs: `extract` gives `ELevel(i)` of a class id (`ulang/prove/simp.u:785`), `node_term` a `false` application (`ulang/prove/egraph.u:146`), and `value_or(…, 0)` at `ulang/prove/translate.u:179, 188, 195` and `ulang/prove/patterns.u:279`.
25. **`measure_slots` takes `acc == 0` as unset** (`ulang/prove/termination.u:193`), so a component with a recursive function of no formals gives a wrong minimum and an `OutOfBounds` in a function.

## 2. Cross-cutting themes

These recur in several packages and are better fixed once each than file by file.

- **Several ways to report an error in the checker**: `report`, direct `s.errors ++ …` (`ulang/check/check.u:1599, 1620, 3830, 3849, 4463, 4504`), `raise` (`declare_name`, `overload`, `declare_sort`, `check_signatures`, `once`, `check_orders`, `exported_aliases`, 3707, 3772), the rewrite in `expect`, which drops all errors since `before`, not only those of unification (2529-2531), and the catch-alls of `each_placed` and `placed_commands`. The direct appends bypass `reaches_error`, `within`, and `unparsed_use`. One `report_at(pos, e)`, and `report` wherever checking can go on.
- **Names encoded in strings**: accessors and updaters `T.f` / `T.f:=` are built and taken apart in five places (`accessor_name`, `updater_name`, `field_of` of `ulang/check/defs.u:178-186`, `without_update` and `field_message` of `ulang/check/message.u:289-302`, `ulang/check/check.u:2312`, `ulang/prove/syntax.u:229`), `field_of` and `without_update` being the same function; `declaring` joins names by `","` into a detail that `declared_by` splits (`ulang/parse/parse.u:131-137, 191-195`); `Name(None, "lambda")` as a sentinel (`ulang/check/raises.u:359`); placeholder modules `Some("?")` and generated names `"(raised)"`, `"(container)"` spelled inline (`ulang/check/check.u:518, 633, 1554, 2058, 3076`) beside `generated_name`. **[Code 3]**
- **Skip-until loops written six times in the parser**: `skip_arg` (612-635) and `skip_stmt` (1574-1588) are near copies, and `parse_expr_recovering`, `parse_example` (by `take(size(start) - size(toks), start)`), and `skip_command` collect `format_token` until a stop. One `skip_until(stop, out text, ref toks)`, which is also where the text cut from the source of **[Diagnostics 1]** would go.
- **The cases of a match, of a handler, and of a split are checked and compiled by copies**: `expr_branches`, `stmt_branches`, `handler_list`, and `proof_cases` of `ulang/check/check.u` (2595, 3279, 3307, 3674); `expr_cases`, `stmt_cases`, and `handler_cases` of `ulang/compile/compile.u` (902-936, 1462-1532). One helper per phase for the scope of a case.
- **The three-case match over `At`** (`AOk`/`AErr` → `that`, `AUnparsed` → its error) is written about nine times in `ulang/syntax/syntax.u` (`definition_in`, `example_error`, `errors_in_*`) and `ulang/check/raises.u` (`stmt_flow`, `expr_errors`). Two generic functions, `node_of[A]` and `errors_at[A]`, would do.
- **Pattern variables have three walks**: `pattern_variables` of `ulang/check/raises.u:300`, `pattern_names` of `ulang/syntax/syntax.u`, and `pat_vars` of `ulang/lint/lint.u:719`.
- **Declared names have three functions, one name with two meanings**: `cmd_names` of `ulang/lint/lint.u:610` (names a command reads) against `cmd_names` of `checks/docs.u:132` (names it declares), beside `declared_name` of `ulang/lint/lint.u:572` and `cmd_name` of `ulang/ulang.u:967`. One `declared_name(cmd)` in `ulang/syntax/syntax.u`.
- **The leading assignments of the locals of a block** are split by the same match in `ulang/format/format.u:732-749` and `ulang/lint/lint.u:404-414`, and possibly by `initialized` of the checker.
- **Stale docs of the syntax before the forms**, `[a, b; xs]`, `{a, b}`, `|a, b|`: `ulang/prelude/list.u:1, 91`, `set.u:41`, `map.u:42`, `array.u:4`, `ulang/prelude/test/containers/containers.u:2, 87`, `ulang/test/datatypes/datatypes.u:165`, `parse_display` and `cons_all` of `ulang/parse/parse.u:672, 683`, `ulang/runtime/ulang.c:737`, `ulang/compile/compile.u:674`; and of the old error nodes, "the parser and the checker make no error nodes yet" in the three case compilers of `ulang/compile/compile.u`, where the reason is now that programs with errors are not compiled.
- **Quadratic growth where a list grows with the program**: `g.lifted ++ List(d)` for every lambda (`lift`, `ulang/compile/compile.u:606-607`), which `CLAUDE.md` asks to be a `Buffer`; `files_of(found)` recomputed per unit and file in the driver; the lints walk every statement once per local of a block (`ulang/lint/lint.u:399-455`); `at(i, from_list(xs))` per element or per access (`ulang/run/run.u:610, 617`, `ulang/prove/simp.u:290, 469`, `ulang/prove/proof.u:104, 118`).
- **Docs on the wrong declaration**, left over from moves: `ulang/check/check.u:505` (of `is_hook`), 3659 (of `is_datatype_type`), 3891, 3163; `ulang/check/defs.u:258`; `ulang/syntax/builtins.u:76, 95`; `ulang/format/format.u:72-74, 584, 1028-1030`; `ulang/lint/lint.u:59-60`; `ulang/ulang.u:756-757`; `ulang/prelude/test/containers/containers.u:103-104`.

## 3. Per package

### `ulang/check/`

- `infer` and `infer_against` (`check.u:1627-1677`) are copies that differ in passing `None` or `Some((t, what))` to `infer_node`; the wrapper that saves `s.at`, runs, takes the errors, and restores is repeated also in `statement`, `normalize_var`/`known_var` (1453-1468, identical but for `normalize` and `known`), `binders`, and twice in `check_proof`.
- `State` of 16 fields is built positionally twice (`start` at 167, only for the test helper `type_of`, and `check_package` at 4447); `check_package` (4430-4493) is 65 lines of four phases with a `local` line of 17 variables. Split into declarations, definitions, and generated tests.
- The three call paths `call_variable`, `overloaded_call`, `declared_call` (3182-3276) repeat the checks of outs and refs, and only `overloaded_call` lacks `same_length`; the `List()` case of `call_declared` calls `declared_call(…, p, p, …)` just to reach `UnknownProcedure`.
- `unify` repeats its report six times (1233-1265); the fallback `CountMismatch("", …)` of `unifies` (1278) is unreachable and would print an empty owner.
- `hidden_sorts` is a `Set[Name]` in `State` and a `List[Name]` in `check_orders`, `check_exposed`, `private_of`, and `check_package`; `is_visible` and `is_visible_sort` (314-328), `visible_names` and `overloads` (221-242), `unique` and `unique_ids` (1303-1317) are twins.
- `exported(sig, values, sorts)` of `sig.u:44, 136` takes the names it removes, `hidden` and `hidden_sorts`, under names that say the opposite; `restrict` and `union` of `sig.u:99-159` make lists of the entries of maps, against Style, and `restrict` drops empty `named` entries that `exported` keeps.
- `defs_of` (`defs.u:85`) runs `ordered_by` three times over all commands; `ordered_by` covers `show` too and is exported but used only inside `defs.u`; `ks`/`k` for constructors (146-176).
- `raises.u`: private `cmd_errors`, `part_errors`, `with_errors` reuse the names of the exported error walks with other meanings; `unchecked_exceptions` filtered in two places (177, 199); `take(1, sort(…))` for a minimum (28); `TSort(t, List())` spelled out five times, an alias `TException(name)`.
- `UnknownProc`/`UnassignedOutput` of `InternalError` against `UnknownProcedure`/`OutputUnassigned` of `TypeError`; the doc of `located_message` (`message.u:8`) still speaks of the position of the command.
- Small: a dead local `u` in `variable` (2909), `LocalTooHigh` of `ts` in `apply` (2341) and `t` in `proof_step` (3551); `nearest`/`is_better` quadratic (274-284); sentinels `unqualified("")` and `""` in `lemma_name`, `first_member`, `param_id` where options fit; `outputs` and `untyped` recurse by hand where a `for` does; the split of formals by mode written twice (1360-1366, 3733-3739); `free` calls `distinct` at every level.
- **[Code 1]**, beyond those listed: `case _` over `Cmd` in `zonk_cmd` (824), `unknown_in_cmd` (966), `command` (3799), `private_in` (3972); over `Expr` in `unknown_in_expr` (906); over `Type` in `has_error_type` (1994), `zonk_type` (626), `datatypes_in` (4789); `subst_pat` of `ulang/syntax/syntax.u` over `Pat`.

### `ulang/syntax/`

- `ulang/syntax/fixtures.u` exports helpers that only the tests of `check.u` use (`lists`, `with_lists`, `program_of`, `natives_prelude`, `type_plain_list`), candidates of overloading in every phase and compiled into `ulang.bin`; and `expr_var`, `expr_int`, `expr_app`, `pat_var` carry the doc "abbreviations … of tests" but are used throughout `run.u` and `check.u`.
- `ulang/syntax/builtins.u`: the aliases hard-code `"prelude/list"` where the functions use `library_name(…)`, with no test that they agree; `array_name()` names `array.from_list`.
- Dead or needlessly exported: `error_kinds` (`error.u:17`, cited only by `CLAUDE.md`), `json_text`, `strip_stmt`.

### `ulang/parse/`

- `parse_command` always returns one command, so `more: List[Cmd]`, the `List(c; rest)` with docs for the rest (3090-3093), and the `List(X; _)` patterns of `enter_section` are vestigial.
- The head of a declaration is parsed four times, in `parse_function`, `parse_pat_alias`, `parse_procedure`, `parse_lemma`, and the "examples, then a signature" rule twice; `ref_befores` and `expected_outs` (2448, 2474) are one function; the `requires` loop (2534-2543) is `parse_separated`.
- Modes are recognized four ways (`at_mode`, `is_mode`, a literal list in `parse_type_atom`, `section_continues`), command keywords three ways (`is_command_token`, `at_command`, `starts_command`), and the section keywords listed in five places. **[Code 2]**
- Stale docs: `parse_stmt_recovering` (1554-1559) describes recovery by columns, which is by tokens; `parse_lemma` (2511-2512) has two heads, one with a comma before `ensures`.
- Dead: the handler of `ParseErrors` in `parse_member` (3000); `ends_body_token` (1536); `starts_at` of `lex.u:232`, which **[Code 4]** can delete rather than move; unused `more` (143) and `imported` (3061, 3128); the unused formal `params` of `parse_plain_name` and `parse_variable`.
- `parse_mod` keeps the misplaced comments reversed ahead of the commands (3062-3109), harmless since diagnostics are sorted.
- Style: token-consuming procedures without `parse_` (`climb*`, `skip_*`, `expect_end`), so either rename them or widen the rule; `at_X` predicates scattered (1504, 1602, 2089, 2554, 3202); `parse_if` tests `TName("elif")` beside `is_elif`; `lex_name` uses `contains(w, keywords())` beside `is_keyword`; `lex` tests `'/'` itself beside `has_slashes_at`; `lex_move_position` lexes nothing.
- `parse_assert` reports a missing `end` at `raises` (1824), every other construct at its opening keyword.

### `ulang/format/`, `ulang/lint/`, driver, checks, Makefile

- The doc "a Var has none" of `ulang/lint/lint.u` (199) is stale, since the lints warn at the formals and locals now (d93a281).
- `format_cmd` never returns `None` (`format.u:805-844`), so the `None` of `format_cmds` (784) is dead; `format_module` is exported but used only in `format.u`; a nested `SBlock` prints as `skip` and its body (626), which would not round-trip and needs a doc of which phase rules it out.
- `ite_text`, `form_text`, and `list_pattern_text` format their children flat and again when spreading (`format.u:324-360, 549-558`), exponential in the depth of spread constructs.
- The lints strip whole subtrees to test a top shape (`literal_bool`, `is_some_of`, `is_plain`, `is_empty_list`, `lint.u:279-476`), where `.that` suffices.
- Driver: `run_target` and `compile_target` share their start (`ulang/ulang.u:297-320`, 539-560), `compile_tests` repeats the loop of `visit_units`, `test_unit` and `prove_unit` are one loop (353-370, 479-492), and the catch of parse errors is written twice (629-638, 724-736, **[Checker 1]**); `strings_of` and `compiled_only_of` match interpreter values by hand (941-956) since the aliases `VCons`, `VStr` of `run.u` are private; `compiled_only_of` loads `project.u` once per directory target.
- Possible mismatch: `test_module` proves no lemmas when names are given (`ulang/ulang.u:421`), while `CLAUDE.md`, Build, says "also the selected ones"; one is stale.
- Makefile: the `.c`/`.bin`/`.sh` triples of `checks/` (139-165) want pattern rules, `selfcompile` and `bootstrap` differ in one step; `find -not -path` still descends into `out/` and `.claude/worktrees/` (110), `-prune`; `$(subst -O2,-O1,$(CFLAGS))` (117) is brittle.
- `checks/termination.u:286`: a one-case match where `.decls` does, and a name without a module prints as `.name`.

### `ulang/run/`, `ulang/compile/`, runtime

- The out `ms` of `compile_cmd`/`compile_cmds` is always empty, collected, and overwritten (`compile.u:1544-1590, 1712-1721`).
- Dead runtime: `u_array_list` and `u_array_put` (`ulang.c:793, 809`, `ulang.h:199, 202`), since `array.u` has no `put`, which `CLAUDE.md`, Library, still names for `OutOfBounds`; `u_fprint_value` (860), unprototyped and non-static; `CeCast` and `CeCond` of `c.u:22-23`, never built.
- `type_key` (`compile.u:356-375`) has cases that `NotAnException` rules out, and a `"?"` fallback that could make two keys equal. **[Code 2]**
- The condition of a `while` is compiled twice (1307-1316); a `CsBreak` gives `while (1) { cond; if (!t) break; body }`.
- `runtime_constructors()` and `runtime_types()` are the same set (150-154); `descriptor_fields` has two cases that differ by the module (180-191); swapped temp names in `SMatch` (1332, 1390).
- Literals rebuilt per use: `LStr` in `pattern_tests` (432, 446), `LUnit`, and `u_mk_bool(1)` for `&u_true`.
- Interpreter: builtins are dispatched by `format_name(name)` built per call and matched against about 30 strings (551, 627, 1260-1265, **[Code 2]**, **[Code 3]**); `show_in` looks up the show twice with an unreachable `None` (839-848); `print_all` loops over a procedure of one argument; the `cache` of a `try` that raises is lost by the atomic effects, which costs the backtracking code; tests `p1`, `p2`, `p4` have numeric indices.
- C: allocations checked in `object`, `compound`, `new_str`, not in `text`, `digits`, `u_push`, `select_file`, `p_os_list` (one checked allocator or `GC_set_oom_fn`); `slow()` copies big operands (110-119) and assumes LP64; `code` and `proc_code` without the prefix `u_`; stale comments at `ulang.h:129` (the empty list) and 214 (`failure_formats`, now `runtime_formats`).

### `ulang/prove/`

The review of the shapes spelled out by hand is fixed since, by f798c04 and 10df7bf.

Other smells:

- Performance: `extract_in` (`simp.u:789-826`) and `smallest` (2253) enumerate every node of every class without memo, on each rewrite; `is_entailed` rebuilds the bounds per call and `variable_division_facts` runs Fourier–Motzkin up to four times per atom; `first_of(List(for s in ss yield f(s)))` (1879, 1917-1929) is `find_map`; `folded` (2249) computes the same round twice under a limit outside `Limits` (**[Code 5]**).
- Copies: `assume_term` (573-583) and `simp_less`/`simp_less_eq` re-encode `comparison_ineq`; orientation of rules in `add_rule`, `merge`, `oriented`; the level equation in `equation_of` and `definition_in`; `division_facts` inlines `is_bound`; the `_seq` and `_concl` twins of `rebind`, `replace`, `subst`, `loose` (145-213), four "map the summands, then `lin_add`", three `*_branch` helpers for optional guards.
- **[Code 1]** beyond `prove/syntax.u`: `case _` over `Term` in `simp_term` (1146), `head_key` (775, where `EBound` and `ELam` share a key), `args_of`, `candidate`, `is_plain`, `term_size`, `calls_in` and `accesses_in` of `termination.u`, `shape` of `egraph.u`.
- Style: `simp_size` and `simp_summands` recurse instead of the traversal; nested tuples passed on (`translate.u:74, 84, 92, 105`, `one_point`, `matches`); `print_pat` and `print_val` of `syntax.u:921-1003` duplicate the printers of `ulang/format` under `print_`, and goals print `[]` and `[a; b]`, which is not ulang; constructors without the prefix of their type (`Open`, `Close`, `Shift`, `Refuted`, `Proven`, `Matched`, `Failed`, `Stuck`); stale docs at `lemma.u:1` ("until proofs have syntax") and `translate.u:12-13` (doubled).

### `ulang/prelude/` and tests

- `CLAUDE.md`, Library, says strings share `exists`, `forall`, and the folds; they have none, `List(for c in "ab")` is `NoFold`. Either give `Str` the folds or narrow the sentence, also in `containers.u:1`.
- Argument orders: `repeat(s, n)` of `str.u:20` against `replicate(n, x)`, which implements it; `join(sep, strs)` against `split(s, c)`; `slice` clamps for lists but is strict for strings and arrays, with no example on `list.slice`, and is re-implemented by `prefix_of` of `compile.u:639`.
- **[Code 5]** twins out of step: `set.u` and `map.u` order their definitions differently, each states half of the precondition of `join`, name their balance tests differently (`map.u`'s `balance` is also a function), and only `map.u` tests rebalancing after removal; the private tree `join` of both overloads `str.join` in the same file.
- Missing helpers that are re-implemented: dropping a suffix after `ends_with` (`cases.u:91`, `parse.u:3407`, `message.u:302`, `checks/docs.u:83, 100, 118`); `keys(m)` (`raises.u:75`, `translate.u:62`, `egraph.u:281`); `path_up` of `ulang.u:912` re-does `relative_path`, and `module_dir` is `os_dirname`. **[Code 4]**
- Missing examples where order or edges are open: `starts_with`, `ends_with`, `take`, `drop`, `zip`, `range`, `index_of`, `remove`, `distinct`, `repeated`, `put`, `insert`, `gcd`, whose example `CLAUDE.md` quotes.
- Stale docs: the module doc of `prelude.u:1-2` names three of the members; `str.u:259` and `list.u:335` say `list.u` cannot use `join` of `str.u`, which members of a package can, so `separated` and `joined_pairs` might become `join`.
- Tests: duplicates (`arrays` of `array.u` repeats its line 55 at 56, `array.u:54` and `containers.u:118`, `set.u:258` and `containers.u:6`, `string_functions` of `expressions.u` and `strings` of `str.u`, the last entry of a key in three places, `List() == List()`); tests in the wrong file or misnamed (`association_lists` and `ranges` of `datatypes.u`, `parse_errors` of `statements.u:395`, `sum_pairs` that sums products, `comparisons` in two files of one feature, `size_positive` that states `>= 0`); `range` of `datatypes.u` and `countdown` of `containers.u` are one helper; `ulang/prove/test/proofs.u` repeats lemmas of `lists.u` without saying that they are facts for later proofs, and the files of `ulang/prove/test/` sit at the top of a `test/`, which `CLAUDE.md`, Tests, forbids while listing the directory, so the rule needs rewording.

## 4. Relation to `TODO.md`

- New items worth adding, as they are not instances of existing ones: the soundness of nested inductions and of lemma outs in calls (1.1.1, 1.1.2), which belong next to Checker 2 and Verification; the normalization of `compare` hooks (1.1.3); structural `==` in the runtime (1.2.6); the exit statuses of the binary of the tests and of `compile --tests` (1.2.7, 1.2.9); the order of evaluation of fused comprehensions (1.2.8).
- **Code 1** lists too few places: the `case _` over `Cmd`, `Expr`, `Type`, `Pat`, and `Term` of section 3 should join it, or the item should say "e.g." and rely on the lint planned in Diagnostics 5.
- **Code 2** and **Code 3** gain the encodings of section 2, of fields, modes, keywords, and generated names, as concrete first steps that need no design.
- **Code 4**: `starts_at` is dead and can go, not move; `min`, `max`, a suffix helper, and `keys` are candidates for the library by the same rule as `split_on`.
- **Diagnostics 1**: the escaping of `format_token` (1.2.17) and the one `skip_until` of section 2 are where the text of `AUnparsed` would change.
- **Diagnostics 5**: the lints should first use the positions of variables that c319a1d added.
- `CLAUDE.md` statements found stale: strings sharing the folds, `put` of arrays, possibly the lemmas of named tests, and the rule of no tests at the top of a `test/` against `ulang/prove/test/`.

## 5. Suggested order

1. The two unsound proofs (1.1.1, 1.1.2), each with a case in `ulang/test/errors/lemmas.u`.
2. `compare` normalization and structural `==` in the runtime (1.1.3, 1.2.6), with cases in `ulang/test/datatypes/orders.u` and `ulang/test/modules/`.
3. The exit statuses and silent paths of the driver and the runtime (1.1.4, 1.2.7, 1.2.9).
4. The small checker and parser bugs (1.2.12-1.2.21), each a few lines with its case.
5. One error-reporting path in the checker, and the string encodings of names (section 2), before Diagnostics 3, which would otherwise build on them.
6. The copies and `case _` of section 3, file by file, each checked by `out/ulang.sh check ulang/ulang.u`.
7. Stale and misplaced docs and the library examples, in one sweep with `checks/docs.u`.
