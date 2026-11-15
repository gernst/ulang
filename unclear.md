# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- `assert raises OsError("cannot write " ++ path) do … end` was a guess: the head of `assert raises` is a pattern, so a message that depends on a value, such as a port that the system chose, is `unknown constructor ++`; such a test catches the exception by a `try` into a variable and asserts on it, as `sockets` of `ulang/prelude/test/os/system.u` does.
- A procedure lambda without formals, `lambda in => p(…) end`, and the type `() raises OsError` of a procedure without formals were guesses that do not parse: CLAUDE.md, Semantics, gives the forms of procedure values only with formals, and says nothing of one without.
