# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A call of a builtin procedure in a scratch program, `os_println(show(y))`, was written without its mode, though every call writes `in` before its ins, also of one argument, `os_println(in show(y))`; the parse error said so.
