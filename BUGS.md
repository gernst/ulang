# Bugs

Known bugs of the tools that ulang depends on, each with how it shows, how it was established, and what follows for the sources.

## C compilers drop the restore of variables after `setjmp`

**How it shows.** The handlers of a `try` see the variables as before it. `compile_try` of `ulang/compile/compile.u` keeps that by copying each variable that the body assigns before `setjmp` and restoring it in the handler:

```c
u_value was_v_toks = v_toks;
struct u_handler *h = u_push();
if (setjmp(h->env) == 0) {
  p_parse_command(v_notation, &v_more, &v_toks);  // returns and moves v_toks
  p_enter_section(v_more, v_pos, &v_section);     // raises
  ...
} else {
  v_toks = was_v_toks;
  ...
  p_skip_command(&v_toks);
}
```

Optimized, the handler sees `v_toks` as the body left it. This hit `parse_mod` of `ulang/parse/parse.u` before c4a5c86, which caught the errors of `parse_command` and `enter_section` in one `try`: in the case `builtin_data` of `ulang/test/errors/modules.u`, the handler skipped one command too many, the line `implementation`, so the definition after it got a second error, at line 12.

**Reproduction.** d5d3d93 with the `try` of `parse_mod` from before c4a5c86 (`git show c4a5c86 -- ulang/parse/parse.u | git apply -R`), built by `make`, and its `out/ulang.c` compiled again with each compiler, then `ulang.sh check` of the text of `builtin_data`:

| Compiler | Result |
|---|---|
| clang 23.1.4 -O0 | correct |
| clang 23.1.4 -O2 | wrong |
| clang 23.1.4 -O2 `-fno-inline` | wrong |
| gcc 16.2.1 -O2 | correct |
| gcc 16.2.1 -O3 | wrong |
| gcc 16.2.1 -O3 `-fno-inline` | correct |

Smaller cases, a ref that a call changes before another call raises, also in a `while`, are compiled correctly by clang -O2, so there is no reduced case yet.

**Cause in clang.** Its assembly of `parse_mod` neither copies `v_toks` into `was_v_toks` before `_setjmp` nor stores it back in the handler, which passes `&v_toks` to `skip_command` as the body left it. The control flow that clang sees has no edge from `longjmp` back to the second return of `setjmp`, so along it the restore stores the value that the copy read, and both are removed. By C11 7.13.2.1, `was_v_toks`, unchanged since `setjmp`, keeps its value, so this is a miscompilation, not undefined behaviour of the generated C. The cause in gcc, which needs inlining, is not established.

**What follows.**

- No `volatile`, see `CLAUDE.md`.
- Code does not rely on the restore: `parse_mod` catches the errors of `parse_command` alone and relies only on a call that raises giving back no outs and refs.
- The assembly backend, `ulang/compile/x86.u`, keeps every variable in a slot of the frame and has no optimizer, so it is not affected.
- Open: a fix of the translation that holds under both compilers, e.g. the saved copies in the handler record instead of C locals, or the variables that a `try` assigns in cells, so that no local changes between `setjmp` and `longjmp`.
