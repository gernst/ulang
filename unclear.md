# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- `pattern` as the name of a variable, the text of a regular expression in `library/regex/regex.u`, is `keyword pattern is not a name`, a parse error at each use: the keywords that are common English words, `pattern`, `example`, `correct`, `invariant`, `induction`, `requires`, and `ensures`, are listed nowhere in one place, and the message gives no other name to write.
- A doc on the first statement after the `local` line of a body, `local x := …` then `// …` then `if …`, parses with the current parser but the seed rejects it, `a comment is the doc of the command or case on the next line`, so `make` fails until `make selfcompile`: `CLAUDE.md` lists "before a `local` line" among the places a doc may not stand, but not that the seed is stricter than the parser after one.
