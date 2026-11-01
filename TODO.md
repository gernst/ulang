# TODO

## The read procedure

1. **Any datatype named `Program` enables `read`**, not only the one of `self/syntax.u`. A program with its own `data Program := Foo(Int)` type-checks `call read(in path, out p)` and then fails at runtime, since `p` is a `Program(…)` from `Quote`.
2. **`read` is reserved** in programs that declare `Program`, so they cannot define their own `read`, although `Run.invoke` would prefer it.
3. **Only missing files are ulang errors.** Other I/O failures, e.g. permissions or a path that `os.Path` rejects, throw Java exceptions that `Main` and `FileTest` do not catch.
4. **Parse errors in the read file** are reported against the reading program, without the read file or the call site, and cannot be caught by `try`.
5. **`self/check.u` does not know `read`**, unlike Check.scala.
6. **`read` is special-cased by name** in `Check.procedure` and `Run.invoke`. Builtin procedures could instead be ordinary signatures in the context, with a table of their implementations in Run.
7. **`Run` has an unused default `dir = os.pwd`**, which invites resolving paths against the working directory.
8. **Wrong arguments to `read`** fall through to the message "unknown procedure read".

## Functions and tuples

1. **`self/` still has the previous semantics**, where functions take several arguments and tuples are nested pairs. `syntax.u` lacks the cases `Type.Fun`, `Type.Tuple`, `Expr.Tuple`, and `Pat.Tuple`, so `Quote` encodes them as the sorts `->` and `*` and the constructor `,`. `self/syntax.u`, `self/check.u`, and `self/run.u` should get these cases and the semantics of Check.scala and Run.scala.
2. **Overloading counts the components of a tuple argument.** `function f(p: Int * Int)` has the same type as `function f(x: Int, y: Int)`, so it counts as a function of two arguments, while a call is resolved by the number of arguments written. Calling an overloaded `f` with one pair `f(p)` therefore does not find it.

## Syntax

Points of a critique of the syntax, ordered by how much they cost when writing `self/check.u` and `self/run.u`.

1. **Records.** Datatypes have only positional fields, so `Ctx` in `self/ctx.u` needs an accessor and an update function per component, each matching `Ctx(a, f, p, d, m, k)`. Named fields would replace them: `c.funs`, `c with funs := …`, `Ctx { funs := f }`.
2. **Locals.** Every result of a procedure call needs a local, declared with a type at the beginning of a block. Inferring the type from an initial value would help: `local x := e`.
3. **Patterns are weaker than expressions.** There are no lists `[x, y]`, guards `case p if c =>`, or-patterns `case TParam(_) | TMeta(_) =>`, or as-patterns, which forced `self/run.u` to rebuild values it had just taken apart.
4. **One global namespace.** Functions, procedures, and constructors share one namespace, and overloading only distinguishes arities, hence names like `zonk_type` and `zonk_expr`, or `UnknownProc` next to `UnknownProcedure`. Qualified names for modules (`check.infer`) or constructors (`Expr.Var`) would help.
5. **Smaller points.**
   - Strings have no escapes and no operations.
   - `assign` is redundant before `x := e`.
   - `[…]` is both type arguments and list literals.

Records, then `local` with an initial value, would shorten `self/check.u` the most.
