# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A call of a builtin procedure in a scratch program, `os_println(show(y))`, was written without its mode, though every call writes `in` before its ins, also of one argument, `os_println(in show(y))`; the parse error said so.
- A procedure lambda without formals, `lambda in => p(…) end`, and the type `() raises OsError` of a procedure without formals were guesses that do not parse: CLAUDE.md, Semantics, gives the forms of procedure values only with formals, and says nothing of one without.
- **A formal named `test` gives a misleading error at its call**: `test` is a keyword, and the call then reports `name expected but , found; modes come in the order …`, which says nothing of the keyword; renaming the formal, to `tester`, fixed it.
- **`catch case ParseError(message, _)` around `parse_file_text` is `UndeclaredRaise`**, since `ParseError` has other constructors, `ImportError` and `ParseErrors`, which the handler leaves raised; `case e => … parse_diagnostic(e).message` covers them all, as `parsed` of `ulang/refactor/refactor.u` does.
- **A module whose sections are all emptied becomes a file without sections**: `format` writes no empty section, so dropping every export of a module leaves only its declarations, without `export` and `implementation`, which then all count as implemented.
- **A pattern variable named `lemma` gives a parse error far from it**, `the for … has no end`: `lemma` is a keyword, and the message names the construct around it, not the name.
- **`check` judges by the built checker, not by the sources**: after an experiment that changed `ulang/check/check.u` and ran `make`, reverting the source left `out/ulang.bin` with the experiment, which kept reporting its errors; and sources halfway through a change of types cannot be built, so a change of the checker's own datatypes is checked by the binary of the commit before, built in a worktree of its own.
- **A type change by text needs word boundaries**: replacing `ms: List[Id]` also changed `params: List[Id]`, which the checker caught only because `Id` and `Path` differ; and `module_text` existed already in `ulang/format/format.u`, for the text of a module from its comments, so a new function of the same name in an imported package overloaded it by arity instead of clashing.
