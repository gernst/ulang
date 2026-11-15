# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A call of a builtin procedure in a scratch program, `os_println(show(y))`, was written without its mode, though every call writes `in` before its ins, also of one argument, `os_println(in show(y))`; the parse error said so.
- A procedure lambda without formals, `lambda in => p(…) end`, and the type `() raises OsError` of a procedure without formals were guesses that do not parse: CLAUDE.md, Semantics, gives the forms of procedure values only with formals, and says nothing of one without.
- **A formal named `test` gives a misleading error at its call**: `test` is a keyword, and the call then reports `name expected but , found; modes come in the order …`, which says nothing of the keyword; renaming the formal, to `tester`, fixed it.
- **`catch case ParseError(message, _)` around `parse_file_text` is `UndeclaredRaise`**, since `ParseError` has other constructors, `ImportError` and `ParseErrors`, which the handler leaves raised; `case e => … parse_diagnostic(e).message` covers them all, as `parsed` of `ulang/refactor/refactor.u` does.
- **A module whose sections are all emptied becomes a file without sections**: `format` writes no empty section, so dropping every export of a module leaves only its declarations, without `export` and `implementation`, which then all count as implemented.
- **A pattern variable named `lemma` gives a parse error far from it**, `the for … has no end`: `lemma` is a keyword, and the message names the construct around it, not the name.
- **`Var.name` is a `Name`, `Id` is `Str`**: a set of the names that an expression reads, `expr_names` of `ulang/lint/lint.u`, is of `Id`, the bases, so a `Var` compares by `x.name.base`; the error was a mismatch of `Set[Str]` and `Set[Name]`.
- **A `(` that starts a statement was taken as the arguments of the name before it**: the parser applies any atom followed by `(`, whatever the layout, so `x := y` then `(a, b) := (b, a)` parsed as `y(a, b)`, and `var q` then `(…)` as the pattern `q(…)`; a tuple assignment needed `at_args` of `ulang/parse/parse.u`, a `(` on the line of the code before it, the first rule of the parser by layout.
- **A multi-assignment built as a tree in a test also depends on its meaning**: the lint that found the assignments that read each other walks parsed sources only, and missed the `sum` of the test `procedures` of `ulang/run/run.u`, whose `SAssign` is built by hand and which then computed 14 instead of 10.
