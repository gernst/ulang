# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- `pattern` as the name of a variable, the text of a regular expression in `library/regex/regex.u`, is `keyword pattern is not a name`, a parse error at each use: the keywords that are common English words, `pattern`, `example`, `correct`, `invariant`, `induction`, `requires`, and `ensures`, are listed nowhere in one place, and the message gives no other name to write.
- A constructor applied to one argument too few in an expression, `AOk(pos, x)` after `AOk` gained a field, is `cannot unify (Pos, Span, ?15) -> At[?15] and (Pos, Pat) -> ?16` at the constructor, while the same in a pattern is `constructor AOk expects 3 arguments, got 2`, followed by `the pattern expects 1 components, got 0` and, for a later case, `unreachable case AOk(pos, EFor(…), _)` with a `_` that the source does not have: the arity is only named for patterns, and the follow-ups of a pattern of the wrong arity are noise (Diagnostics 10 has the arity of functions; the follow-ups are not listed).
- A call of a procedure with one in too many, `call field_access(in env, pos, span, …)` against a signature without `span`, is `expects 5 ins, got 6` and then `expects 0 values, got 1`, a second count of something else, and a mismatch for each argument after the extra one, shifted by one.
- In the parser, a `match (literal_at(toks), head_token(toks))` has cases `(_, TKw("let"))` that look like the elements of `Tokens`, so a rewrite of the token tuples by their shape also rewrote these pairs; the checker caught it.
