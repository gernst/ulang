# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- `assert raises OsError("cannot write " ++ path) do … end` was a guess: the head of `assert raises` is a pattern, so a message that depends on a value, such as a port that the system chose, is `unknown constructor ++`; such a test catches the exception by a `try` into a variable and asserts on it, as `sockets` of `ulang/prelude/test/os/system.u` does.
- A procedure lambda without formals, `lambda in => p(…) end`, and the type `() raises OsError` of a procedure without formals were guesses that do not parse: CLAUDE.md, Semantics, gives the forms of procedure values only with formals, and says nothing of one without.
- **A formal named `test` gives a misleading error at its call**: `test` is a keyword, and the call then reports `name expected but , found; modes come in the order …`, which says nothing of the keyword; renaming the formal, to `tester`, fixed it.
- **`catch case ParseError(message, _)` around `parse_file_text` is `UndeclaredRaise`**, since `ParseError` has other constructors, `ImportError` and `ParseErrors`, which the handler leaves raised; `case e => … parse_diagnostic(e).message` covers them all, as `parsed` of `ulang/refactor/refactor.u` does.
- **A module whose sections are all emptied becomes a file without sections**: `format` writes no empty section, so dropping every export of a module leaves only its declarations, without `export` and `implementation`, which then all count as implemented.
