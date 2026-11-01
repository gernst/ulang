package ulang

// the syntax as values of the datatypes of self/syntax.u and back, without positions
object Quote:
  def obj(c: Name, args: Any*): Any =
    Obj(c, args.toList)

  def list[A](xs: List[A])(f: A => Any): Any =
    xs.foldRight(obj("Nil"))((x, v) => obj("::", f(x), v))

  def option[A](o: Option[A])(f: A => Any): Any =
    o match
      case Some(a) =>
        obj("Some", f(a))
      case None =>
        obj("None")

  def names(xs: List[Name]): Any =
    list(xs)(x => x)

  def quote(t: Type): Any =
    t match
      case Type.Hole =>
        obj("THole")
      case Type.Meta(index) =>
        obj("TMeta", BigInt(index))
      case Type.Param(name) =>
        obj("TParam", name)
      case Type.Sort(name, args) =>
        obj("TSort", name, list(args)(quote))
      case Type.Fun(args, res) =>
        obj("TFun", list(args)(quote), quote(res))
      case Type.Tuple(items) =>
        obj("TTuple", list(items)(quote))

  def value(v: Any): Any =
    v match
      case () =>
        obj("VUnit")
      case b: Boolean =>
        obj("VBool", b)
      case n: BigInt =>
        obj("VInt", n)
      case s: String =>
        obj("VStr", s)
      case _ =>
        error("runtime", s"invalid literal $v")

  def variable(name: Name, typ: Type): Any =
    obj("Var", name, quote(typ))

  def vars(xs: List[Expr.Var]): Any =
    list(xs)(x => variable(x.name, x.typ))

  def binders(xs: List[Pat.Var]): Any =
    list(xs)(x => variable(x.name, x.typ))

  def quote(e: Expr): Any =
    e match
      case Expr.Lit(v) =>
        obj("ELit", value(v))
      case Expr.Var(name, typ) =>
        obj("EVar", variable(name, typ))
      case Expr.App(fun, args) =>
        obj("EApp", quote(fun), list(args)(quote))
      case Expr.Tuple(items) =>
        obj("ETuple", list(items)(quote))
      case Expr.Lam(bound, body) =>
        obj("ELam", binders(bound), quote(body))
      case Expr.Let(bound, body) =>
        obj("ELet", list(bound)((x, a) => obj(",", variable(x.name, x.typ), quote(a))), quote(body))
      case Expr.Ite(test, left, right) =>
        obj("EIte", quote(test), quote(left), quote(right))
      case Expr.Match(arg, cases) =>
        obj("EMatch", quote(arg), list(cases)(c => obj("Branch", quote(c.pat), option(c.guard)(quote), quote(c.body))))
      case Expr.Comp(result, bound, lists, test) =>
        obj("EComp", quote(result), binders(bound), list(lists)(quote), quote(test))

  def quote(p: Pat): Any =
    p match
      case Pat.Wild() =>
        obj("PWild")
      case Pat.Lit(v) =>
        obj("PLit", value(v))
      case Pat.Var(name, typ) =>
        obj("PVar", variable(name, typ))
      case Pat.App(name, args) =>
        obj("PApp", name, list(args)(quote))
      case Pat.Tuple(items) =>
        obj("PTuple", list(items)(quote))

  def quote(s: Stmt): Any =
    s match
      case Stmt.Skip() =>
        obj("SSkip")
      case Stmt.Assign(pairs) =>
        obj("SAssign", list(pairs)((x, a) => obj(",", variable(x.name, x.typ), quote(a))))
      case Stmt.Block(locals, stmts) =>
        obj("SBlock", binders(locals), list(stmts)(quote))
      case Stmt.If(test, left, right) =>
        obj("SIf", quote(test), quote(left), quote(right))
      case Stmt.While(test, body) =>
        obj("SWhile", quote(test), quote(body))
      case Stmt.Call(proc, ins, outs, refs) =>
        obj("SCall", proc, list(ins)(quote), vars(outs), vars(refs))
      case Stmt.Match(arg, cases) =>
        obj("SMatch", quote(arg), list(cases)(c => obj("Branch", quote(c.pat), option(c.guard)(quote), quote(c.body))))
      case Stmt.Assert(test) =>
        obj("SAssert", quote(test))
      case Stmt.Raise(arg, typ) =>
        obj("SRaise", quote(arg), quote(typ))
      case Stmt.Try(body, handlers) =>
        obj("STry", quote(body), list(handlers)(h => obj("Handler", quote(h.typ), quote(h.pat), option(h.guard)(quote), quote(h.body))))

  def quote(f: Fixity): Any =
    f match
      case Fixity.Prefix =>
        obj("FPrefix")
      case Fixity.Postfix =>
        obj("FPostfix")
      case Fixity.Infix =>
        obj("FInfix")
      case Fixity.Left =>
        obj("FLeft")
      case Fixity.Right =>
        obj("FRight")

  def quote(c: Cmd): Any =
    c match
      case Cmd.Sort(name, params, typ) =>
        obj("CSort", name, names(params), quote(typ))
      case Cmd.Data(name, params, constrs) =>
        obj("CData", name, names(params), list(constrs)(k => obj("Constr", k.name, list(k.args)(quote))))
      case Cmd.Fun(name, params, bound, res, body) =>
        obj("CFun", name, names(params), list(bound)(binders), quote(res), quote(body))
      case Cmd.Proc(name, params, ins, outs, refs, body) =>
        obj("CProc", name, names(params), binders(ins), binders(outs), binders(refs), quote(body))
      case Cmd.Import(name) =>
        obj("CImport", name)
      case Cmd.Notation(fixity, prec, ops) =>
        obj("CNotation", quote(fixity), BigInt(prec), names(ops))
      case Cmd.Eval(expr) =>
        obj("CEval", quote(expr))
      case Cmd.Run(body) =>
        obj("CRun", quote(body))
      case Cmd.Test(name, body) =>
        obj("CTest", name, quote(body))

  def quote(p: Program): Any =
    obj("Program", list(p.cmds)(quote))

  def invalid(what: String, v: Any): Nothing =
    error("runtime", s"${Printer.show(v)} is not $what")

  def items[A](v: Any)(f: Any => A): List[A] =
    v match
      case Obj("Nil", Nil) =>
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
      case Obj("THole", Nil) =>
        Type.Hole
      case Obj("TMeta", List(index)) =>
        Type.Meta(int(index))
      case Obj("TParam", List(n)) =>
        Type.Param(name(n))
      case Obj("TFun", List(args, res)) =>
        Type.Fun(items(args)(typ), typ(res))
      case Obj("TTuple", List(ts)) =>
        Type.Tuple(items(ts)(typ))
      case Obj("TSort", List(n, args)) =>
        Type.Sort(name(n), items(args)(typ))
      case _ =>
        invalid("a type", v)

  def literal(v: Any): Any =
    v match
      case Obj("VUnit", Nil) =>
        ()
      case Obj("VBool", List(b: Boolean)) =>
        b
      case Obj("VInt", List(n: BigInt)) =>
        n
      case Obj("VStr", List(s: String)) =>
        s
      case _ =>
        invalid("a literal", v)

  def variable(v: Any): Expr.Var =
    v match
      case Obj("Var", List(n, t)) =>
        Expr.Var(name(n), typ(t))()
      case _ =>
        invalid("a variable", v)

  def binder(v: Any): Pat.Var =
    v match
      case Obj("Var", List(n, t)) =>
        Pat.Var(name(n), typ(t))()
      case _ =>
        invalid("a variable", v)

  def pair[A, B](first: Any => A, second: Any => B)(v: Any): (A, B) =
    v match
      case Obj(",", List(a, b)) =>
        (first(a), second(b))
      case _ =>
        invalid("a pair", v)

  def optional[A](v: Any)(f: Any => A): Option[A] =
    v match
      case Obj("Some", List(a)) =>
        Some(f(a))
      case Obj("None", Nil) =>
        None
      case _ =>
        invalid("an option", v)

  def branch[A](v: Any)(body: Any => A): Case[A] =
    v match
      case Obj("Branch", List(p, g, b)) =>
        Case(pat(p), optional(g)(expr), body(b))
      case _ =>
        invalid("a case", v)

  def expr(v: Any): Expr =
    v match
      // the unit value of self/syntax.u is the empty tuple
      case Obj("ELit", List(Obj("VUnit", Nil))) =>
        Expr.Tuple(Nil)()
      case Obj("ELit", List(l)) =>
        Expr.Lit(literal(l))()
      case Obj("EVar", List(x)) =>
        variable(x)
      case Obj("ETuple", List(es)) =>
        Expr.Tuple(items(es)(expr))()
      case Obj("EApp", List(fun, args)) =>
        Expr.App(expr(fun), items(args)(expr))()
      case Obj("ELam", List(bound, body)) =>
        Expr.Lam(items(bound)(binder), expr(body))()
      case Obj("ELet", List(bound, body)) =>
        Expr.Let(items(bound)(pair(binder, expr)), expr(body))()
      case Obj("EIte", List(test, left, right)) =>
        Expr.Ite(expr(test), expr(left), expr(right))()
      case Obj("EMatch", List(arg, cases)) =>
        Expr.Match(expr(arg), items(cases)(branch(_)(expr)))()
      case Obj("EComp", List(result, bound, lists, test)) =>
        Expr.Comp(expr(result), items(bound)(binder), items(lists)(expr), expr(test))()
      case _ =>
        invalid("an expression", v)

  def pat(v: Any): Pat =
    v match
      case Obj("PWild", Nil) =>
        Pat.Wild()()
      case Obj("PLit", List(Obj("VUnit", Nil))) =>
        Pat.Tuple(Nil)()
      case Obj("PLit", List(l)) =>
        Pat.Lit(literal(l))()
      case Obj("PVar", List(x)) =>
        binder(x)
      case Obj("PTuple", List(ps)) =>
        Pat.Tuple(items(ps)(pat))()
      case Obj("PApp", List(n, args)) =>
        Pat.App(name(n), items(args)(pat))()
      case _ =>
        invalid("a pattern", v)

  def handler(v: Any): Handler =
    v match
      case Obj("Handler", List(t, p, g, body)) =>
        Handler(typ(t), pat(p), optional(g)(expr), stmt(body))
      case _ =>
        invalid("a handler", v)

  def stmt(v: Any): Stmt =
    v match
      case Obj("SSkip", Nil) =>
        Stmt.Skip()()
      case Obj("SAssign", List(pairs)) =>
        Stmt.Assign(items(pairs)(pair(variable, expr)))()
      case Obj("SBlock", List(locals, stmts)) =>
        Stmt.Block(items(locals)(binder), items(stmts)(stmt))()
      case Obj("SIf", List(test, left, right)) =>
        Stmt.If(expr(test), stmt(left), stmt(right))()
      case Obj("SWhile", List(test, body)) =>
        Stmt.While(expr(test), stmt(body))()
      case Obj("SCall", List(proc, ins, outs, refs)) =>
        Stmt.Call(name(proc), items(ins)(expr), items(outs)(variable), items(refs)(variable))()
      case Obj("SMatch", List(arg, cases)) =>
        Stmt.Match(expr(arg), items(cases)(branch(_)(stmt)))()
      case Obj("SAssert", List(test)) =>
        Stmt.Assert(expr(test))()
      case Obj("SRaise", List(arg, t)) =>
        Stmt.Raise(expr(arg), typ(t))()
      case Obj("STry", List(body, handlers)) =>
        Stmt.Try(stmt(body), items(handlers)(handler))()
      case _ =>
        invalid("a statement", v)

  def fixity(v: Any): Fixity =
    v match
      case Obj("FPrefix", Nil) =>
        Fixity.Prefix
      case Obj("FPostfix", Nil) =>
        Fixity.Postfix
      case Obj("FInfix", Nil) =>
        Fixity.Infix
      case Obj("FLeft", Nil) =>
        Fixity.Left
      case Obj("FRight", Nil) =>
        Fixity.Right
      case _ =>
        invalid("a fixity", v)

  def constr(v: Any): Constr =
    v match
      case Obj("Constr", List(n, args)) =>
        Constr(name(n), items(args)(typ))
      case _ =>
        invalid("a constructor", v)

  def cmd(v: Any): Cmd =
    v match
      case Obj("CSort", List(n, params, t)) =>
        Cmd.Sort(name(n), items(params)(name), typ(t))()
      case Obj("CData", List(n, params, constrs)) =>
        Cmd.Data(name(n), items(params)(name), items(constrs)(constr))()
      case Obj("CFun", List(n, params, bound, res, body)) =>
        Cmd.Fun(name(n), items(params)(name), items(bound)(items(_)(binder)), typ(res), expr(body))()
      case Obj("CProc", List(n, params, ins, outs, refs, body)) =>
        Cmd.Proc(name(n), items(params)(name), items(ins)(binder), items(outs)(binder), items(refs)(binder), stmt(body))()
      case Obj("CImport", List(n)) =>
        Cmd.Import(name(n))()
      case Obj("CNotation", List(f, prec, ops)) =>
        Cmd.Notation(fixity(f), int(prec), items(ops)(name))()
      case Obj("CEval", List(e)) =>
        Cmd.Eval(expr(e))()
      case Obj("CRun", List(body)) =>
        Cmd.Run(stmt(body))()
      case Obj("CTest", List(n, body)) =>
        Cmd.Test(name(n), stmt(body))()
      case _ =>
        invalid("a command", v)

  def unquote(v: Any): Program =
    v match
      case Obj("Program", List(cmds)) =>
        Program(items(cmds)(cmd))
      case _ =>
        invalid("a program", v)
