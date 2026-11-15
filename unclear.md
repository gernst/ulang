# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- **`ideas/refactor.md` said that docs survive edits since they are syntax**: comments are not in the tree but `Mod.comments`, placed back by spans, so an edit of the tree loses them; the prototype of `refactor` splices the text of declarations instead.
- **`return` in a function body has no hint**: `function f(x: Int): Int :=` then `return x + 1` is `declaration expected but x found` at the `x`, while in a procedure body `return` gets the hint that a procedure assigns its outs; in an expression `return` parses as a name.
- **Tests that parse files from disk take about 30 seconds each in the interpreter**, since each parse loads the library: the scripts of `refactor` run in `ulang/refactor/test/`, only compiled.
- **`CLAUDE.md` names `append(b, x)` of `buffer`, which does not exist**: `ulang/prelude/buffer.u` has only `b ++ xs` of a list, so one element is `b ++ List(x)`.
