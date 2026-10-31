package ulang

// the syntax as values of the datatypes of self/syntax.u and back, without positions
object Quote:
  def obj(c: Name, args: Any*): Any =
    Obj(c, args.toList)

  def list[A](xs: List[A])(f: A => Any): Any =
    xs.foldRight(obj("nil"))((x, v) => obj("::", f(x), v))

  def names(xs: List[Name]): Any =
    list(xs)(x => x)

  def quote(t: Type): Any =
    t match
      case Type.Hole =>
        obj("thole")
      case Type.Meta(index) =>
        obj("tmeta", BigInt(index))
      case Type.Param(name) =>
        obj("tparam", name)
      case Type.Sort(name, args) =>
        obj("tsort", name, list(args)(quote))

  def value(v: Any): Any =
    v match
      case () =>
        obj("vunit")
      case b: Boolean =>
        obj("vbool", b)
      case n: BigInt =>
        obj("vint", n)
      case s: String =>
        obj("vstr", s)
      case _ =>
        error("runtime", s"invalid literal $v")

  def variable(name: Name, typ: Type): Any =
    obj("var", name, quote(typ))

  def vars(xs: List[Expr.Var]): Any =
    list(xs)(x => variable(x.name, x.typ))

  def binders(xs: List[Pat.Var]): Any =
    list(xs)(x => variable(x.name, x.typ))

  def quote(e: Expr): Any =
    e match
      case Expr.Lit(v) =>
        obj("elit", value(v))
      case Expr.Var(name, typ) =>
        obj("evar", variable(name, typ))
      case Expr.App(fun, args) =>
        obj("eapp", quote(fun), list(args)(quote))
      case Expr.Lam(bound, body) =>
        obj("elam", binders(bound), quote(body))
      case Expr.Let(bound, args, body) =>
        obj("elet", binders(bound), list(args)(quote), quote(body))
      case Expr.Ite(test, left, right) =>
        obj("eite", quote(test), quote(left), quote(right))
      case Expr.Match(arg, cases) =>
        obj("ematch", quote(arg), list(cases)(c => obj("branch", quote(c.pat), quote(c.body))))
      case Expr.Comp(result, bound, lists, test) =>
        obj("ecomp", quote(result), binders(bound), list(lists)(quote), quote(test))

  def quote(p: Pat): Any =
    p match
      case Pat.Wild() =>
        obj("pwild")
      case Pat.Lit(v) =>
        obj("plit", value(v))
      case Pat.Var(name, typ) =>
        obj("pvar", variable(name, typ))
      case Pat.App(name, args) =>
        obj("papp", name, list(args)(quote))

  def quote(s: Stmt): Any =
    s match
      case Stmt.Skip() =>
        obj("sskip")
      case Stmt.Assign(vars, args) =>
        obj("sassign", this.vars(vars), list(args)(quote))
      case Stmt.Block(locals, stmts) =>
        obj("sblock", binders(locals), list(stmts)(quote))
      case Stmt.If(test, left, right) =>
        obj("sif", quote(test), quote(left), quote(right))
      case Stmt.While(test, body) =>
        obj("swhile", quote(test), quote(body))
      case Stmt.Call(proc, ins, outs, refs) =>
        obj("scall", proc, list(ins)(quote), vars(outs), vars(refs))
      case Stmt.Match(arg, cases) =>
        obj("smatch", quote(arg), list(cases)(c => obj("branch", quote(c.pat), quote(c.body))))
      case Stmt.Assert(test) =>
        obj("sassert", quote(test))
      case Stmt.Raise(arg, typ) =>
        obj("sraise", quote(arg), quote(typ))
      case Stmt.Try(body, handlers) =>
        obj("stry", quote(body), list(handlers)(h => obj("handler", quote(h.typ), quote(h.pat), quote(h.body))))

  def quote(f: Fixity): Any =
    f match
      case Fixity.Prefix =>
        obj("fprefix")
      case Fixity.Postfix =>
        obj("fpostfix")
      case Fixity.Infix =>
        obj("finfix")
      case Fixity.Left =>
        obj("fleft")
      case Fixity.Right =>
        obj("fright")

  def quote(c: Cmd): Any =
    c match
      case Cmd.Sort(name, params, typ) =>
        obj("csort", name, names(params), quote(typ))
      case Cmd.Data(name, params, constrs) =>
        obj("cdata", name, names(params), list(constrs)(k => obj("constr", k.name, list(k.args)(quote))))
      case Cmd.Fun(name, params, bound, res, body) =>
        obj("cfun", name, names(params), binders(bound), quote(res), quote(body))
      case Cmd.Proc(name, params, ins, outs, refs, body) =>
        obj("cproc", name, names(params), binders(ins), binders(outs), binders(refs), quote(body))
      case Cmd.Import(name) =>
        obj("cimport", name)
      case Cmd.Notation(fixity, prec, ops) =>
        obj("cnotation", quote(fixity), BigInt(prec), names(ops))
      case Cmd.Eval(expr) =>
        obj("ceval", quote(expr))
      case Cmd.Run(body) =>
        obj("crun", quote(body))
      case Cmd.Test(body) =>
        obj("ctest", quote(body))

  def quote(p: Program): Any =
    obj("program", list(p.cmds)(quote))

  def invalid(what: String, v: Any): Nothing =
    error("runtime", s"${Printer.show(v)} is not $what")

  def items[A](v: Any)(f: Any => A): List[A] =
    v match
      case Obj("nil", Nil) =>
        Nil
      case Obj("::", List(x, xs)) =>
        f(x) :: items(xs)(f)
      case _ =>
        invalid("a list", v)

  def name(v: Any): Name =
    v match
      case s: String =>
        s
      case _ =>
        invalid("a name", v)

  def int(v: Any): Int =
    v match
      case n: BigInt =>
        n.toInt
      case _ =>
        invalid("an integer", v)

  def typ(v: Any): Type =
    v match
      case Obj("thole", Nil) =>
        Type.Hole
      case Obj("tmeta", List(index)) =>
        Type.Meta(int(index))
      case Obj("tparam", List(n)) =>
        Type.Param(name(n))
      case Obj("tsort", List(n, args)) =>
        Type.Sort(name(n), items(args)(typ))
      case _ =>
        invalid("a type", v)

  def literal(v: Any): Any =
    v match
      case Obj("vunit", Nil) =>
        ()
      case Obj("vbool", List(b: Boolean)) =>
        b
      case Obj("vint", List(n: BigInt)) =>
        n
      case Obj("vstr", List(s: String)) =>
        s
      case _ =>
        invalid("a literal", v)

  def variable(v: Any): Expr.Var =
    v match
      case Obj("var", List(n, t)) =>
        Expr.Var(name(n), typ(t))()
      case _ =>
        invalid("a variable", v)

  def binder(v: Any): Pat.Var =
    v match
      case Obj("var", List(n, t)) =>
        Pat.Var(name(n), typ(t))()
      case _ =>
        invalid("a variable", v)

  def branch[A](v: Any)(body: Any => A): Case[A] =
    v match
      case Obj("branch", List(p, b)) =>
        Case(pat(p), body(b))
      case _ =>
        invalid("a case", v)

  def expr(v: Any): Expr =
    v match
      case Obj("elit", List(l)) =>
        Expr.Lit(literal(l))()
      case Obj("evar", List(x)) =>
        variable(x)
      case Obj("eapp", List(fun, args)) =>
        Expr.App(expr(fun), items(args)(expr))()
      case Obj("elam", List(bound, body)) =>
        Expr.Lam(items(bound)(binder), expr(body))()
      case Obj("elet", List(bound, args, body)) =>
        Expr.Let(items(bound)(binder), items(args)(expr), expr(body))()
      case Obj("eite", List(test, left, right)) =>
        Expr.Ite(expr(test), expr(left), expr(right))()
      case Obj("ematch", List(arg, cases)) =>
        Expr.Match(expr(arg), items(cases)(branch(_)(expr)))()
      case Obj("ecomp", List(result, bound, lists, test)) =>
        Expr.Comp(expr(result), items(bound)(binder), items(lists)(expr), expr(test))()
      case _ =>
        invalid("an expression", v)

  def pat(v: Any): Pat =
    v match
      case Obj("pwild", Nil) =>
        Pat.Wild()()
      case Obj("plit", List(l)) =>
        Pat.Lit(literal(l))()
      case Obj("pvar", List(x)) =>
        binder(x)
      case Obj("papp", List(n, args)) =>
        Pat.App(name(n), items(args)(pat))()
      case _ =>
        invalid("a pattern", v)

  def handler(v: Any): Handler =
    v match
      case Obj("handler", List(t, p, body)) =>
        Handler(typ(t), pat(p), stmt(body))
      case _ =>
        invalid("a handler", v)

  def stmt(v: Any): Stmt =
    v match
      case Obj("sskip", Nil) =>
        Stmt.Skip()()
      case Obj("sassign", List(vars, args)) =>
        Stmt.Assign(items(vars)(variable), items(args)(expr))()
      case Obj("sblock", List(locals, stmts)) =>
        Stmt.Block(items(locals)(binder), items(stmts)(stmt))()
      case Obj("sif", List(test, left, right)) =>
        Stmt.If(expr(test), stmt(left), stmt(right))()
      case Obj("swhile", List(test, body)) =>
        Stmt.While(expr(test), stmt(body))()
      case Obj("scall", List(proc, ins, outs, refs)) =>
        Stmt.Call(name(proc), items(ins)(expr), items(outs)(variable), items(refs)(variable))()
      case Obj("smatch", List(arg, cases)) =>
        Stmt.Match(expr(arg), items(cases)(branch(_)(stmt)))()
      case Obj("sassert", List(test)) =>
        Stmt.Assert(expr(test))()
      case Obj("sraise", List(arg, t)) =>
        Stmt.Raise(expr(arg), typ(t))()
      case Obj("stry", List(body, handlers)) =>
        Stmt.Try(stmt(body), items(handlers)(handler))()
      case _ =>
        invalid("a statement", v)

  def fixity(v: Any): Fixity =
    v match
      case Obj("fprefix", Nil) =>
        Fixity.Prefix
      case Obj("fpostfix", Nil) =>
        Fixity.Postfix
      case Obj("finfix", Nil) =>
        Fixity.Infix
      case Obj("fleft", Nil) =>
        Fixity.Left
      case Obj("fright", Nil) =>
        Fixity.Right
      case _ =>
        invalid("a fixity", v)

  def constr(v: Any): Constr =
    v match
      case Obj("constr", List(n, args)) =>
        Constr(name(n), items(args)(typ))
      case _ =>
        invalid("a constructor", v)

  def cmd(v: Any): Cmd =
    v match
      case Obj("csort", List(n, params, t)) =>
        Cmd.Sort(name(n), items(params)(name), typ(t))()
      case Obj("cdata", List(n, params, constrs)) =>
        Cmd.Data(name(n), items(params)(name), items(constrs)(constr))()
      case Obj("cfun", List(n, params, bound, res, body)) =>
        Cmd.Fun(name(n), items(params)(name), items(bound)(binder), typ(res), expr(body))()
      case Obj("cproc", List(n, params, ins, outs, refs, body)) =>
        Cmd.Proc(name(n), items(params)(name), items(ins)(binder), items(outs)(binder), items(refs)(binder), stmt(body))()
      case Obj("cimport", List(n)) =>
        Cmd.Import(name(n))()
      case Obj("cnotation", List(f, prec, ops)) =>
        Cmd.Notation(fixity(f), int(prec), items(ops)(name))()
      case Obj("ceval", List(e)) =>
        Cmd.Eval(expr(e))()
      case Obj("crun", List(body)) =>
        Cmd.Run(stmt(body))()
      case Obj("ctest", List(body)) =>
        Cmd.Test(stmt(body))()
      case _ =>
        invalid("a command", v)

  def unquote(v: Any): Program =
    v match
      case Obj("program", List(cmds)) =>
        Program(items(cmds)(cmd))
      case _ =>
        invalid("a program", v)
