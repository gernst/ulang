package ulang

// params are the type parameters in scope, which distinguish Param from Sort,
// notations are the operators declared so far, loaded are the modules imported so far,
// and read gives the text of a module
case class Scope(params: Set[String], notations: List[source.Cmd.Notation], loaded: Set[String], read: String => String):
  def prefix(op: String): Option[Int] =
    notations.collectFirst:
      case source.Cmd.Notation(source.Fixity.Prefix, prec, names, _) if names.contains(op) =>
        prec

  def postfix(op: String): Option[Int] =
    notations.collectFirst:
      case source.Cmd.Notation(source.Fixity.Postfix, prec, names, _) if names.contains(op) =>
        prec

  def infix(op: String): Option[(source.Fixity, Int)] =
    notations.collectFirst:
      case source.Cmd.Notation(fixity @ (source.Fixity.Infix | source.Fixity.Left | source.Fixity.Right), prec, names, _)
          if names.contains(op) =>
        (fixity, prec)

  def declared(op: String): Boolean =
    notations.exists(_.names.contains(op))

// resolves operators, sugar, and imports of the source syntax, giving the core syntax
object Resolve:
  def fail(msg: String, pos: source.Pos): Nothing =
    throw new Error(s"parse error: $msg at $pos")

  def hole(x: String): core.Pat.Var =
    core.Pat.Var(x, core.Type.Hole)

  def op(f: String, args: core.Expr*): core.Expr =
    core.Expr.App(core.Expr.Var(f, core.Type.Hole), args.toList)

  // the commands of cs, and the scope after them, where notations and imports apply to the commands after them
  def cmds(s: Scope, cs: List[source.Cmd]): (List[core.Cmd], Scope) =
    cs match
      case Nil =>
        (Nil, s)
      case c :: rest =>
        val (cs1, s1) = cmd(s, c)
        val (cs2, s2) = cmds(s1, rest)
        (cs1 ++ cs2, s2)

  def cmd(s: Scope, c: source.Cmd): (List[core.Cmd], Scope) =
    c match
      case source.Cmd.Import(n, _) if s.loaded(n) =>
        (Nil, s)
      case source.Cmd.Import(n, _) =>
        val text = s.read(n)
        val (cs, s1) =
          try cmds(s.copy(loaded = s.loaded + n), source.Parser.parse(text).cmds)
          catch
            case e: Error =>
              throw new Error(s"${e.getMessage} in $n")
        (core.Cmd.Import(n) :: cs, s1)
      case n @ source.Cmd.Notation(fixity, prec, names, _) =>
        (List(core.Cmd.Notation(core.Fixity.valueOf(fixity.toString), prec, names)), s.copy(notations = s.notations :+ n))
      case _ =>
        (List(declaration(s, c)), s)

  def declaration(s: Scope, c: source.Cmd): core.Cmd =
    c match
      case source.Cmd.Sort(n, ps, t, _) =>
        core.Cmd.Sort(n, ps, typ(s.copy(params = ps.toSet), t))
      case source.Cmd.Data(n, ps, cs, _) =>
        val s1 = s.copy(params = ps.toSet)
        core.Cmd.Data(n, ps, cs.map(c => core.Constr(c.name, c.args.map(typ(s1, _)))))
      case source.Cmd.Fun(n, ps, xs, r, b, _) =>
        val s1 = s.copy(params = ps.toSet)
        core.Cmd.Fun(n, ps, xs.map(formal(s1, _)), typ(s1, r), expr(s1, b))
      case source.Cmd.Proc(n, ps, ins, outs, refs, b, _) =>
        val s1 = s.copy(params = ps.toSet)
        core.Cmd.Proc(n, ps, ins.map(formal(s1, _)), outs.map(formal(s1, _)), refs.map(formal(s1, _)), stmt(s1, b))
      case source.Cmd.Eval(e, _) =>
        core.Cmd.Eval(expr(s, e))
      case source.Cmd.Run(b, _) =>
        core.Cmd.Run(stmt(s, b))
      case source.Cmd.Test(b, _) =>
        core.Cmd.Test(stmt(s, b))
      case source.Cmd.Import(_, pos) =>
        fail("unexpected import", pos)
      case source.Cmd.Notation(_, _, _, pos) =>
        fail("unexpected notation", pos)

  def formal(s: Scope, x: source.Pat.Var): core.Pat.Var =
    core.Pat.Var(x.name, x.typ.map(typ(s, _)).getOrElse(core.Type.Hole))

  def variable(x: source.Expr.Var): core.Expr.Var =
    core.Expr.Var(x.name, core.Type.Hole)

  // (a) is just a, and (a, b, c) is (a, (b, c)), since tuples are binary, and () is Unit
  def pair(ts: List[core.Type]): core.Type =
    if ts.isEmpty
    then core.Type.Sort("Unit", Nil)
    else ts.reduceRight((a, b) => core.Type.Sort("*", List(a, b)))

  def typ(s: Scope, t: source.Type): core.Type =
    pair(types(s, t))

  // a type, or the types of a parenthesized list
  def types(s: Scope, t: source.Type): List[core.Type] =
    t match
      case source.Type.Sort(n, Nil, _) if s.params(n) =>
        List(core.Type.Param(n))
      case source.Type.Sort(n, args, _) =>
        List(core.Type.Sort(n, args.map(typ(s, _))))
      case source.Type.Tuple(ts, _) =>
        ts.map(typ(s, _))
      case source.Type.Ops(items, pos) =>
        Climb[List[core.Type]](
          s,
          pos,
          (op, t) => List(core.Type.Sort(op, List(pair(t)))),
          (op, t) => List(core.Type.Sort(op, List(pair(t)))),
          // for ->, a parenthesized list on the left are the arguments of a function
          (op, l, r) =>
            if op == "->"
            then List(core.Type.Sort("->", l :+ pair(r)))
            else List(core.Type.Sort(op, List(pair(l), pair(r))))
        ).all(symbolic(items).map(value(types(s, _))))

  def expr(s: Scope, e: source.Expr): core.Expr =
    e match
      case source.Expr.Lit(v, _) =>
        core.Expr.Lit(v)
      case x: source.Expr.Var =>
        variable(x)
      case source.Expr.App(f, args, _) =>
        core.Expr.App(expr(s, f), args.map(expr(s, _)))
      case source.Expr.Lam(xs, b, _) =>
        core.Expr.Lam(xs.map(formal(s, _)), expr(s, b))
      case source.Expr.Let(ps, es, b, pos) =>
        let(ps.map(pat(s, _)), es.map(expr(s, _)), expr(s, b), pos)
      case source.Expr.Ite(c, l, r, _) =>
        core.Expr.Ite(expr(s, c), expr(s, l), expr(s, r))
      case source.Expr.Match(arg, cases, _) =>
        core.Expr.Match(expr(s, arg), cases.map(c => core.Case(pat(s, c.pat), expr(s, c.body))))
      case source.Expr.Comp(r, xs, ls, t, _) =>
        core.Expr.Comp(expr(s, r), xs.map(formal(s, _)), ls.map(expr(s, _)), t.map(expr(s, _)).getOrElse(core.Expr.Lit(true)))
      case source.Expr.Tuple(es, _) =>
        es.map(expr(s, _)).reduceRight(op(",", _, _))
      case source.Expr.Elems(es, _) =>
        es.map(expr(s, _)).foldRight(core.Expr.Var("nil", core.Type.Hole): core.Expr)(op("::", _, _))
      case source.Expr.Ops(items, pos) =>
        Climb[core.Expr](s, pos, op(_, _), op(_, _), op(_, _, _)).all(words(s, items).map(value(expr(s, _))))

  // variables are bound in parallel, other patterns are matched against the tuple of values
  def let(ps: List[core.Pat], es: List[core.Expr], b: core.Expr, pos: source.Pos): core.Expr =
    val xs = ps.collect:
      case x: core.Pat.Var =>
        x
    if xs.length == ps.length
    then core.Expr.Let(xs, es, b)
    else if ps.length == es.length
    then core.Expr.Match(es.reduceRight(op(",", _, _)), List(core.Case(ps.reduceRight((a, b) => core.Pat.App(",", List(a, b))), b)))
    else fail(s"let expects ${ps.length} values, got ${es.length}", pos)

  def pat(s: Scope, p: source.Pat): core.Pat =
    p match
      case source.Pat.Wild(_) =>
        core.Pat.Wild
      case source.Pat.Lit(v, _) =>
        core.Pat.Lit(v)
      case x: source.Pat.Var =>
        formal(s, x)
      case source.Pat.App(c, ps, _) =>
        core.Pat.App(c, ps.map(pat(s, _)))
      case source.Pat.Tuple(ps, _) =>
        ps.map(pat(s, _)).reduceRight((a, b) => core.Pat.App(",", List(a, b)))
      case source.Pat.Ops(items, pos) =>
        Climb[core.Pat](
          s,
          pos,
          (op, p) => core.Pat.App(op, List(p)),
          (op, p) => core.Pat.App(op, List(p)),
          (op, l, r) => core.Pat.App(op, List(l, r))
        ).all(symbolic(items).map(value(pat(s, _))))

  def stmt(s: Scope, st: source.Stmt): core.Stmt =
    st match
      case source.Stmt.Skip(_) =>
        core.Stmt.Skip
      case source.Stmt.Assign(xs, es, _) =>
        core.Stmt.Assign(xs.map(variable), es.map(expr(s, _)))
      case source.Stmt.Block(locals, ss, _) =>
        core.Stmt.Block(locals.map(formal(s, _)), ss.map(stmt(s, _)))
      case source.Stmt.If(c, l, r, _) =>
        core.Stmt.If(expr(s, c), stmt(s, l), stmt(s, r))
      case source.Stmt.While(c, b, _) =>
        core.Stmt.While(expr(s, c), stmt(s, b))
      case source.Stmt.Call(p, ins, outs, refs, _) =>
        core.Stmt.Call(p, ins.map(expr(s, _)), outs.map(variable), refs.map(variable))
      case source.Stmt.Match(arg, cases, _) =>
        core.Stmt.Match(expr(s, arg), cases.map(c => core.Case(pat(s, c.pat), stmt(s, c.body))))
      case source.Stmt.Assert(c, _) =>
        core.Stmt.Assert(expr(s, c))

  def value[A, B](f: A => B)(t: source.Item[A]): source.Item[B] =
    t match
      case source.Item.Arg(a) =>
        source.Item.Arg(f(a))
      case source.Item.Op(op, pos) =>
        source.Item.Op(op, pos)

  def symbolic[A](items: List[source.Item[A]]): List[source.Item[A]] =
    items.map:
      case source.Item.Arg(a) =>
        source.Item.Arg(a)
      case source.Item.Op(op, pos) =>
        source.Item.Op(op, pos)

  // names declared as operators are operators, where op(e) is op applied to (e)
  def words(s: Scope, items: List[source.Item[source.Expr]]): List[source.Item[source.Expr]] =
    items.flatMap:
      case source.Item.Op(op, pos) =>
        List(source.Item.Op(op, pos))
      case source.Item.Arg(source.Expr.Var(x, pos)) if s.declared(x) =>
        List(source.Item.Op(x, pos))
      case source.Item.Arg(source.Expr.App(source.Expr.Var(x, pos), args, p)) if s.declared(x) =>
        List(source.Item.Op(x, pos), source.Item.Arg(source.Expr.Tuple(args, p)))
      case source.Item.Arg(e) =>
        List(source.Item.Arg(e))

// precedence climbing, where prefix operators bind their operand with their own precedence,
// and pos is the start of the tokens
case class Climb[A](s: Scope, pos: source.Pos, prefix: (String, A) => A, postfix: (String, A) => A, infix: (String, A, A) => A):
  def all(tokens: List[source.Item[A]]): A =
    climb(tokens, 0) match
      case (a, Nil) =>
        a
      case (_, source.Item.Op(op, p) :: _) =>
        Resolve.fail(s"unexpected operator $op", p)
      case (_, _) =>
        Resolve.fail("missing operator", pos)

  def climb(tokens: List[source.Item[A]], min: Int): (A, List[source.Item[A]]) =
    val (lhs, rest) = operand(tokens)
    loop(lhs, rest, min)

  def operand(tokens: List[source.Item[A]]): (A, List[source.Item[A]]) =
    tokens match
      case source.Item.Arg(a) :: rest =>
        (a, rest)
      case source.Item.Op(op, _) :: rest if s.prefix(op).nonEmpty =>
        val (a, rest1) = climb(rest, s.prefix(op).get)
        (prefix(op, a), rest1)
      case source.Item.Op(op, p) :: _ if s.declared(op) =>
        Resolve.fail(s"$op is not a prefix operator", p)
      case source.Item.Op(op, p) :: _ =>
        Resolve.fail(s"undeclared operator $op", p)
      case Nil =>
        Resolve.fail("missing operand", pos)

  def loop(lhs: A, tokens: List[source.Item[A]], min: Int): (A, List[source.Item[A]]) =
    tokens match
      case source.Item.Op(op, _) :: rest if s.postfix(op).exists(_ >= min) =>
        loop(postfix(op, lhs), rest, min)
      case source.Item.Op(op, p) :: rest if s.infix(op).exists(_._2 >= min) =>
        val (fixity, prec) = s.infix(op).get
        val (rhs, rest1) = climb(rest, if fixity == source.Fixity.Right then prec else prec + 1)
        nonassociative(op, p, fixity, prec, rest1)
        loop(infix(op, lhs, rhs), rest1, min)
      case source.Item.Op(op, p) :: _ if !s.declared(op) =>
        Resolve.fail(s"undeclared operator $op", p)
      case _ =>
        (lhs, tokens)

  def nonassociative(op: String, pos: source.Pos, fixity: source.Fixity, prec: Int, rest: List[source.Item[A]]): Unit =
    rest match
      case source.Item.Op(next, _) :: _ if fixity == source.Fixity.Infix && s.infix(next).exists(_._2 == prec) =>
        Resolve.fail(s"operator $op is not associative", pos)
      case _ =>
