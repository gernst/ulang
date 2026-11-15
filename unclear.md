# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A call of a builtin procedure in a scratch program, `os_println(show(y))`, was written without its mode, though every call writes `in` before its ins, also of one argument, `os_println(in show(y))`; the parse error said so.
- A procedure lambda without formals, `lambda in => p(…) end`, and the type `() raises OsError` of a procedure without formals were guesses that do not parse: CLAUDE.md, Semantics, gives the forms of procedure values only with formals, and says nothing of one without.
- **A formal named `test` gives a misleading error at its call**: `test` is a keyword, and the call then reports `name expected but , found; modes come in the order …`, which says nothing of the keyword; renaming the formal, to `tester`, fixed it.
- **`catch case ParseError(message, _)` around `parse_file_text` is `UndeclaredRaise`**, since `ParseError` has other constructors, `ImportError` and `ParseErrors`, which the handler leaves raised; `case e => … parse_diagnostic(e).message` covers them all, as `parsed` of `ulang/refactor/refactor.u` does.
- **A module whose sections are all emptied becomes a file without sections**: `format` writes no empty section, so dropping every export of a module leaves only its declarations, without `export` and `implementation`, which then all count as implemented.
- **A pattern variable named `lemma` gives a parse error far from it**, `the for … has no end`: `lemma` is a keyword, and the message names the construct around it, not the name.
- **A walk named `sorts_in` in `ulang/prove/translate.u` was ambiguous** with `sorts_in` of `ulang/syntax/syntax.u`, which the prover imports and which already gives the names of the types in a type, as a list; overloading by type made each use ambiguous instead of hiding the import.
- **`format` does not rewrap comments**: a doc edited longer than 120 columns stays so, and a replacement that split a comment line left a line starting with `:`, a parse error; comments are wrapped by hand.
- **A regression test of an order passed without the fix**: `measure_slots` of `ulang/prove/termination.u` only fails when the function without formals comes first in the component, which the names order, so the case names it `base` before `count`; a case of a bug that depends on an order is checked against the code before the fix.
