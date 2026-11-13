# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A `//` doc above the `match` that is the whole body of a function was rejected by `format`, `a comment is the doc of the command or case on the next line`: the body of a function is an expression, which takes no doc, so the note goes on a `case`; `CLAUDE.md` lists the places of docs but not that a body of a function is none of them.
- `data` is a keyword, so a formal named `data` does not parse; the rule that a keyword is no name of a field or a local is in Syntax 6 and Documentation 2, not in `CLAUDE.md`.
- A parse error in the head of a procedure skips the whole command, so a second error in its body is not reported: the parser resumes at the next statement only inside a body that it entered, which `CLAUDE.md`, Writing ulang, does not say.
- A hint of `hint_of` of `ulang/check/message.u` is no `Hinted` of `ulang/check/check.u`, so a case that reads the hint of a `Hinted` finds none for it, and tests the message by `located_message` instead.
- A case of `ulang/test/errors/` runs alone in the interpreter, `out/ulang.sh test ulang/test/errors/types.u:<name>`, in about 12 to 30 s, though `CLAUDE.md` says that they run only compiled, which holds for the walk of `test`, not for a file named.
- The message of `OpenLemma` of a gap at a step of a proof ends with ` at L.C`, which only the cases of `ulang/test/errors/lemmas.u` show.
- `Seq` and `Concl`, the sequents of the prover, are declared in `ulang/prove/simp.u`, not in `ulang/prove/syntax.u` with the terms.
- A target that is not a directory was dropped by `targets_of` of `ulang/ulang.u`, by its filter of sources, not by `walk_all`, which the review named.
- `pattern` as the name of a variable, the text of a regular expression in `library/regex/regex.u`, is `keyword pattern is not a name`, a parse error at each use: the keywords that are common English words, `pattern`, `example`, `correct`, `invariant`, `induction`, `requires`, and `ensures`, are listed nowhere in one place, and the message gives no other name to write.
- A doc on the first statement after the `local` line of a body, `local x := …` then `// …` then `if …`, parses with the current parser but the seed rejects it, `a comment is the doc of the command or case on the next line`, so `make` fails until `make selfcompile`: `CLAUDE.md` lists "before a `local` line" among the places a doc may not stand, but not that the seed is stricter than the parser after one.
- **A pair has no field `first`**: `binding.first.pos` is `UnknownFieldType` at `.first`, since tuples have no fields; a pair is taken apart by `first(p)` or a pattern, and a pattern variable named `first` hides the function, so `first(second)` inside `case ELet(List(first, second; more), …)` was `cannot unify … and … -> ?`.
- **`ulang.sh run` and `check` are the built binary**: after an edit of the checker, a sample run by `out/ulang.sh run` still had the old semantics until `make`; and `check ulang/ulang.u` passed with a parse error inside a test of `ulang/prove/patterns.u`, a member of a package it imports, since imports drop the tests of the modules they load, so a merge of a member is checked by checking that member.
- **Generators have no `while`**: `List(for d in stmts while not is_local(d.at) yield d)` does not parse; a prefix is a recursive function until `take_while` (Library 2 of `ideas/library.md`).
