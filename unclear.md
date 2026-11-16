# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A procedure lambda without formals, `lambda in => p(…) end`, and the type `() raises OsError` of a procedure without formals were guesses that do not parse: CLAUDE.md, Semantics, gives the forms of procedure values only with formals, and says nothing of one without.
- **`Var.name` is a `Name`, `Id` is `Str`**: a set of the names that an expression reads, `expr_names` of `ulang/lint/lint.u`, is of `Id`, the bases, so a `Var` compares by `x.name.base`; the error was a mismatch of `Set[Str]` and `Set[Name]`.
- **A test file for `@fix` without an entry or exports loses its declarations**: `UnusedPrivate` fires on every function and its fix drops them, which hides the edits under test.
- **`normal_file(here, here, root)` with the root as the working directory gives the absolute root**, not `.`, so it compares with no relative path.
- **The compilers call a function directly only if the type of its name has the arity of the call** (`arity(ty) == size(args)` in `compile_named` of `ulang/compile/compile.u`), so code that an expansion makes after checking needs a function type of the right arity on its names, `TFun` of holes in `form_call` of `ulang/expand/expand.u`, and `THole` makes every such call `u_apply`.
- **The reply of `run_script` keeps a refusal of several lines as one element**, `List("rejected", "…\n…")`, so a test that expects the lines of a long refusal compares one string with newlines, not a list of lines (`patches_after_drops` of `ulang/refactor/test/declarations.u`).
- **A `@patch` of one declaration formats the whole file**, so an unformatted layout of another declaration of a test's sample, such as a call broken over two lines, is joined by the first patch that succeeds; a case that needs the broken layout runs before it (`patches_parts_of_lines` of `ulang/refactor/test/declarations.u`).
- **The tests of `ulang/refactor/test/` keep the journal of their directory between runs**: a test that writes only an empty `handles.txt` and expects `script 1 of the journal` passes once, then sees `script 2`, so an expectation leaves out the line of the journal.
- **`check_unit` of `ulang/ulang.u` proves every file of a unit, also those without lemmas**: `selection` of `ulang/ulang.u` gives `Some(List())` for a phase without names, so each file translated the whole program for the prover until `prover_of` of `ulang/test/runner.u` shared it, and the contracts of the program are roots of termination for every file, not only for those with lemmas.
