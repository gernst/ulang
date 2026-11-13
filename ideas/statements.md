# Statements without call and assign, and locals by let and var

The plan of the branch `bindings`, which changes the statements of procedures in one change, deleted when it is on `main`.

Done so far: the bindings of a `let` are sequential, and a tuple binds in parallel, `let (a, b) := (b, a) in …`; a `local`
stands anywhere in a body, as the statement `SLocal` of `ulang/syntax/syntax.u`, destructures by an irrefutable pattern,
and binds sequentially; and every pattern may have a type, `p: T`, `PAnnotated` of `ulang/syntax/syntax.u`, also the
formals of lambdas, `lambda p: Point => p.x`. The checker gives the backends what they had: one binding per `let`, and
blocks that declare their locals at their start, where a local of a pattern is assigned by a match (`declared` of
`ulang/check/check.u`).

Open, in this order:

1. **`let` and `var` in procedures**, in place of `local`: `let p := e, …` binds values that nothing assigns, by
   patterns, each a value, as the `let` of expressions binds; `var x := e, y: T, z` declares variables, which may be
   assigned, passed as outs and refs, and declared without a value, as `local` does now. `SLocal(mutable, decls)`; the
   checker leaves the variables of a `let` out of what the rest of its body may assign. `var` becomes a keyword, so the
   fields and variables named `var` are renamed first.
2. **No `call` and no `assign`**: a statement that starts with a name is a call `p(in a, out b)` if `(` follows, and an
   assignment `x := e, y := f` if `:=` or `,` follows, simultaneous as now, so the grammar needs two tokens, and a
   statement still ends at what ends an expression followed by a name that is no operator (`at_next` of
   `ulang/parse/parse.u`). Also in proofs: the steps `l(in a, out b)` of lemmas and `t := s`. The hints for habits of
   other languages change with it: `x += 1` is `write x := x + 1`, and `x = 1` `write x := 1`. `call` and `assign` are
   names again, as is `local`. The printer drops the padding of calls next to assignments.
3. **The seed**: the parser accepts the old and the new forms, the printer writes the new ones, and `make selfcompile`
   replaces the seed; then `make reformat` rewrites the sources, where a temporary rule of the printer writes a
   `local` that nothing assigns after its value as `let` and the others as `var`, in the order of their declarations;
   the texts of cases in strings are rewritten by a script, with their expected positions taken from runs; then the old
   forms go.
4. **Docs**: `CLAUDE.md`, `README.md`, and `ideas/`, also the entry of `ideas/rejected.md` that kept `call` and
   `assign`, and Syntax 5 and 6.
5. `make test` once, then the merge into `main`.

`elif` is deferred.
