# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- **`ideas/refactor.md` said that docs survive edits since they are syntax**: comments are not in the tree but `Mod.comments`, placed back by spans, so an edit of the tree loses them; the prototype of `refactor` splices the text of declarations instead.
- **`return` in a function body has no hint**: `function f(x: Int): Int :=` then `return x + 1` is `declaration expected but x found` at the `x`, while in a procedure body `return` gets the hint that a procedure assigns its outs; in an expression `return` parses as a name.
- **Tests that parse files from disk take about 30 seconds each in the interpreter**, since each parse loads the library: the scripts of `refactor` run in `ulang/refactor/test/`, only compiled.
- **`keyword _ is not a name` at an out of a call comes from `expected` of `ulang/parse/parse.u`**, reached through `parse_plain_name` of `parse_variable`, and `parse_sections` took one item procedure for outs and refs alike, so an `_` only for outs needed an item of its own per section (`item_out` and `item_ref`).
- **The outs and refs of an `SCall` are plain `Var`s without positions**, as `CLAUDE.md` says under `At`, which is easy to miss: a lint cannot place an edit at an out, so the warning of `UnusedVariable` for a local that only receives outs has no edit that writes `_`.
- **A scratch program wrote `os_println(show(n))`**, the habit of other languages, and overloaded `pair` of `Int` and `Str` in one file, which is `AlreadyDefined`, since within a module overloading is only by arity; overloads by type need two members of a package.
