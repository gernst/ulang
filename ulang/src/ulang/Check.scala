package ulang

object Check:
  val Bool = Type.Sort("Bool", Nil)
  val Int = Type.Sort("Int", Nil)
  val Str = Type.Sort("Str", Nil)
  val Char = Type.Sort("Char", Nil)
  val Empty = Type.Tuple(Nil)

  val basic = Set("Bool", "Int", "Str", "Char")

  def fun(args: List[Type], res: Type): Type =
    Type.Fun(args, res)

  // the type of a function with several lists of parameters, which returns functions for all but the first
  def curried(argss: List[List[Type]], res: Type): Type =
    argss.foldRight(res)(fun)

  // type schemes are types with the Params of a generic declaration or the Metas generalized by a let
  def mono(t: Type): (List[Type], Type) =
    (Nil, t)

  // the type scheme of a constructor, which is a function if it has arguments
  def scheme(k: (List[Type], List[Type], Type)): (List[Type], Type) =
    k match
      case (params, Nil, res) =>
        (params, res)
      case (params, args, res) =>
        (params, fun(args, res))

  val initial: Ctx =
    Ctx()

  // the number of arguments of a function, functions are overloaded by it
  def arity(t: Type): Int =
    t match
      case Type.Fun(args, _) =>
        args.length
      case _ =>
        -1

  // only what cannot be defined in the prelude
  val builtins: Map[Name, (List[Type], Type)] =
    val arith = mono(fun(List(Int, Int), Int))
    val logic = mono(fun(List(Bool, Bool), Bool))
    val eq = (List(Type.Param("A")), fun(List(Type.Param("A"), Type.Param("A")), Bool))
    Map(
      "+" -> arith,
      "-" -> arith,
      "*" -> arith,
      "/" -> arith,
      "%" -> arith,
      "<" -> mono(fun(List(Int, Int), Bool)),
      "==" -> eq,
      "and" -> logic,
      "or" -> logic,
      // strings are converted from and to lists of characters
      "chars" -> mono(fun(List(Str), Type.Sort("List", List(Char)))),
      "string" -> mono(fun(List(Type.Sort("List", List(Char))), Str)),
      "ord" -> mono(fun(List(Char), Int)),
      "chr" -> mono(fun(List(Int), Char)),
      // operations on strings and characters, which are built in for speed
      "^" -> mono(fun(List(Str, Str), Str)),
      "str_size" -> mono(fun(List(Str), Int)),
      "str_join" -> mono(fun(List(Str, Type.Sort("List", List(Str))), Str)),
      "str_slice" -> mono(fun(List(Str, Int, Int), Str)),
      "str_split" -> mono(fun(List(Str, Char), Type.Sort("List", List(Str)))),
      "str_show" -> mono(fun(List(Int), Str)),
      "str_number" -> mono(fun(List(Str), Type.Sort("Option", List(Int)))),
      "is_digit" -> mono(fun(List(Char), Bool)),
      "is_letter" -> mono(fun(List(Char), Bool)),
      "is_space" -> mono(fun(List(Char), Bool))
    )


  def show(t: Type): String =
    t match
      case Type.Param(name) =>
        name
      case Type.Hole =>
        "_"
      case Type.Meta(index) =>
        s"?$index"
      // a function of the empty tuple, unlike one without arguments () -> r
      case Type.Fun(List(Type.Tuple(Nil)), r) =>
        s"(()) -> ${show(r)}"
      case Type.Fun(List(a), r) =>
        s"${operand(a)} -> ${show(r)}"
      case Type.Fun(args, r) =>
        args.map(show(_)).mkString("(", ", ", ") -> ") + show(r)
      case Type.Tuple(Nil) =>
        "()"
      case Type.Tuple(ts) =>
        ts.map(component).mkString(" * ")
      case Type.Sort(name, Nil) =>
        name
      case Type.Sort(name, args) =>
        args.map(show(_)).mkString(s"$name[", ", ", "]")

  // the argument of a function, in parentheses if it is a function
  def operand(t: Type): String =
    t match
      case Type.Fun(_, _) =>
        s"(${show(t)})"
      case _ =>
        show(t)

  // a component of a tuple, in parentheses if it is a function or a tuple
  def component(t: Type): String =
    t match
      case Type.Fun(_, _) | Type.Tuple(_ :: _ :: _) =>
        s"(${show(t)})"
      case _ =>
        show(t)

  // the Params and Metas in t
  def free(t: Type): List[Type] =
    t match
      case Type.Param(_) | Type.Meta(_) =>
        List(t)
      case Type.Hole =>
        Nil
      case Type.Sort(_, args) =>
        args.flatMap(free).distinct
      case Type.Fun(args, r) =>
        (args :+ r).flatMap(free).distinct
      case Type.Tuple(ts) =>
        ts.flatMap(free).distinct

  def holes(t: Type): Boolean =
    t match
      case Type.Hole =>
        true
      case Type.Sort(_, args) =>
        args.exists(holes)
      case Type.Fun(args, r) =>
        (args :+ r).exists(holes)
      case Type.Tuple(ts) =>
        ts.exists(holes)
      case _ =>
        false

  def subst(t: Type, su: Map[Type, Type]): Type =
    t match
      case Type.Sort(name, args) =>
        Type.Sort(name, args.map(subst(_, su)))
      case Type.Fun(args, r) =>
        Type.Fun(args.map(subst(_, su)), subst(r, su))
      case Type.Tuple(ts) =>
        Type.Tuple(ts.map(subst(_, su)))
      case _ =>
        su.getOrElse(t, t)

  def program(p: Program): Program =
    new Check(p).cmds()

  def expr(e: Expr): (Expr, Type) =
    val c = new Check(Program(Nil))
    val (e1, t) = c.infer(Map(), e)
    (c.zonk(e1), c.zonk(t))

class Check(program: Program):
  import Check.*

  val context = Ref(initial)
  val su = Ref(Map[Int, Type]())
  val next = Ref(0)

  def aliases =
    context.get.aliases
  def funs =
    context.get.funs
  def procs =
    context.get.procs
  def datas =
    context.get.datas
  def members =
    context.get.members
  def constrs =
    context.get.constrs

  def update(f: Ctx => Ctx): Unit =
    context.get = f(context.get)

  def fresh(): Type =
    next.get += 1
    Type.Meta(next.get)

  def distinct(names: List[Name], what: String): Unit =
    val dups = names.diff(names.distinct)
    require("type", dups.isEmpty, s"duplicate $what ${dups.distinct.mkString(", ")}")

  def sameLength(xs: List[?], es: List[?], what: String): Unit =
    require("type", xs.length == es.length, s"$what expects ${xs.length} values, got ${es.length}")

  def arguments(name: Name, arity: Int, args: List[Type]): Unit =
    require("type", arity == args.length, s"sort $name expects $arity arguments, got ${args.length}")

  // expands sort definitions and replaces holes by fresh unknowns
  def normalize(t: Type): Type =
    t match
      case Type.Hole =>
        fresh()
      case Type.Param(_) | Type.Meta(_) =>
        t
      case Type.Fun(args, r) =>
        Type.Fun(args.map(normalize), normalize(r))
      case Type.Tuple(ts) =>
        Type.Tuple(ts.map(normalize))
      case Type.Sort(name, args) if basic(name) =>
        arguments(name, 0, args)
        t
      case Type.Sort(name, args) if datas.contains(name) =>
        arguments(name, datas(name), args)
        Type.Sort(name, args.map(normalize))
      case Type.Sort(name, args) =>
        val (params, rhs) = aliases.getOrElse(name, error("type", s"unknown sort $name"))
        arguments(name, params.length, args)
        subst(rhs, params.map(Type.Param(_)).zip(args.map(normalize)).toMap)

  def known(t: Type, what: String): Type =
    require("type", !holes(t), s"missing type for $what")
    normalize(t)

  def formals(xs: List[Pat.Var]): List[Pat.Var] =
    distinct(xs.names, "variable")
    xs.map(x => Pat.Var(x.name, normalize(x.typ))(x.pos))

  def signature(xs: List[Pat.Var]): List[Pat.Var] =
    distinct(xs.names, "variable")
    xs.map(x => at(x.pos)(Pat.Var(x.name, known(x.typ, x.name))(x.pos)))

  def bind(xs: List[Pat.Var]): Map[Name, (List[Type], Type)] =
    xs.map(x => x.name -> mono(x.typ)).toMap

  def resolve(t: Type): Type =
    t match
      case Type.Meta(index) if su.get.contains(index) =>
        resolve(su.get(index))
      case _ =>
        t

  def zonk(t: Type): Type =
    resolve(t) match
      case Type.Sort(name, args) =>
        Type.Sort(name, args.map(zonk(_)))
      case Type.Fun(args, r) =>
        Type.Fun(args.map(zonk(_)), zonk(r))
      case Type.Tuple(ts) =>
        Type.Tuple(ts.map(zonk(_)))
      case t =>
        t

  def zonk(x: Expr.Var): Expr.Var =
    Expr.Var(x.name, zonk(x.typ))(x.pos)

  def zonk(x: Pat.Var): Pat.Var =
    Pat.Var(x.name, zonk(x.typ))(x.pos)

  def zonk(e: Expr): Expr =
    e match
      case Expr.Lit(_) =>
        e
      case x: Expr.Var =>
        zonk(x)
      case Expr.App(fun, args) =>
        Expr.App(zonk(fun), args.map(zonk(_)))(e.pos)
      case Expr.Lam(bound, body) =>
        Expr.Lam(bound.map(zonk(_)), zonk(body))(e.pos)
      case Expr.Let(bound, body) =>
        Expr.Let(bound.map((x, a) => (zonk(x), zonk(a))), zonk(body))(e.pos)
      case Expr.Ite(test, left, right) =>
        Expr.Ite(zonk(test), zonk(left), zonk(right))(e.pos)
      case Expr.Match(arg, cases) =>
        Expr.Match(zonk(arg), cases.map(c => Case(zonk(c.pat), c.guard.map(zonk(_)), zonk(c.body))))(e.pos)
      case Expr.Comp(result, bound, lists, test) =>
        Expr.Comp(zonk(result), bound.map(zonk(_)), lists.map(zonk(_)), zonk(test))(e.pos)
      case Expr.Tuple(items) =>
        Expr.Tuple(items.map(zonk(_)))(e.pos)

  def zonk(p: Pat): Pat =
    p match
      case Pat.Wild() | Pat.Lit(_) =>
        p
      case Pat.Var(name, typ) =>
        Pat.Var(name, zonk(typ))(p.pos)
      case Pat.App(name, args) =>
        Pat.App(name, args.map(zonk(_)))(p.pos)
      case Pat.Tuple(items) =>
        Pat.Tuple(items.map(zonk(_)))(p.pos)

  def zonk(s: Stmt): Stmt =
    s match
      case Stmt.Skip() =>
        s
      case Stmt.Assign(pairs) =>
        Stmt.Assign(pairs.map((x, a) => (zonk(x), zonk(a))))(s.pos)
      case Stmt.Block(locals, stmts) =>
        Stmt.Block(locals.map(zonk(_)), stmts.map(zonk(_)))(s.pos)
      case Stmt.If(test, left, right) =>
        Stmt.If(zonk(test), zonk(left), zonk(right))(s.pos)
      case Stmt.While(test, body) =>
        Stmt.While(zonk(test), zonk(body))(s.pos)
      case Stmt.Call(proc, ins, outs, refs) =>
        Stmt.Call(proc, ins.map(zonk(_)), outs.map(zonk(_)), refs.map(zonk(_)))(s.pos)
      case Stmt.Match(arg, cases) =>
        Stmt.Match(zonk(arg), cases.map(c => Case(zonk(c.pat), c.guard.map(zonk(_)), zonk(c.body))))(s.pos)
      case Stmt.Assert(test) =>
        Stmt.Assert(zonk(test))(s.pos)
      case Stmt.Raise(arg, typ) =>
        Stmt.Raise(zonk(arg), at(s.pos)(exception(typ)))(s.pos)
      case Stmt.Try(body, handlers) =>
        Stmt.Try(zonk(body), handlers.map(zonk(_)))(s.pos)

  def zonk(h: Handler): Handler =
    h.pat match
      case Pat.Wild() =>
        Handler(zonk(h.typ), h.pat, h.guard.map(zonk(_)), zonk(h.body))
      case _ =>
        Handler(at(h.pat.pos)(exception(h.typ)), zonk(h.pat), h.guard.map(zonk(_)), zonk(h.body))

  // exceptions are told apart by their types at runtime, which must therefore be known after checking
  def exception(t: Type): Type =
    val u = zonk(t)
    require("type", free(u).isEmpty, s"the type ${shows(List(u)).head} of an exception is not fully known")
    u

  def zonk(d: Cmd): Cmd =
    d match
      case Cmd.Sort(_, _, _) | Cmd.Data(_, _, _) =>
        d
      case Cmd.Fun(name, params, bound, res, body) =>
        Cmd.Fun(name, params, bound.map(_.map(zonk(_))), zonk(res), zonk(body))(d.pos)
      case Cmd.Proc(name, params, ins, outs, refs, body) =>
        Cmd.Proc(name, params, ins.map(zonk(_)), outs.map(zonk(_)), refs.map(zonk(_)), zonk(body))(d.pos)
      case Cmd.Import(_) | Cmd.Notation(_, _, _) =>
        d
      case Cmd.Eval(expr) =>
        Cmd.Eval(zonk(expr))(d.pos)
      case Cmd.Run(body) =>
        Cmd.Run(zonk(body))(d.pos)
      case Cmd.Test(name, body) =>
        Cmd.Test(name, zonk(body))(d.pos)

  def occurs(i: Int, t: Type): Boolean =
    resolve(t) match
      case Type.Meta(index) =>
        i == index
      case Type.Sort(_, args) =>
        args.exists(occurs(i, _))
      case Type.Fun(args, r) =>
        (args :+ r).exists(occurs(i, _))
      case Type.Tuple(ts) =>
        ts.exists(occurs(i, _))
      case _ =>
        false

  def unify(t1: Type, t2: Type): Unit =
    (resolve(t1), resolve(t2)) match
      case (Type.Meta(a), Type.Meta(b)) if a == b =>
        ()
      case (Type.Param(a), Type.Param(b)) if a == b =>
        ()
      case (Type.Meta(index), t) =>
        assign(index, t)
      case (t, Type.Meta(index)) =>
        assign(index, t)
      case (Type.Sort(f, as), Type.Sort(g, bs)) if f == g && as.length == bs.length =>
        unify(as, bs)
      case (Type.Fun(as, r), Type.Fun(bs, s)) if as.length == bs.length =>
        unify(as :+ r, bs :+ s)
      case (Type.Tuple(as), Type.Tuple(bs)) if as.length == bs.length =>
        unify(as, bs)
      case _ =>
        val List(a, b) = shows(List(t1, t2)): @unchecked
        error("type", s"cannot unify $a and $b")

  def unify(as: List[Type], bs: List[Type]): Unit =
    (as, bs) match
      case (Nil, Nil)         =>
      case (a :: as, b :: bs) =>
        unify(a, b);
        unify(as, bs)
      case _ =>
        error("type", s"cannot unify ${as.length} types with ${bs.length} types")

  def assign(i: Int, t: Type): Unit =
    require("type", !occurs(i, t), s"infinite type ${shows(List(Type.Meta(i), t)).mkString(" = ")}")
    su.get += i -> t

  // shows types for a message, with their unknowns numbered from 1 in order of appearance
  def shows(ts: List[Type]): List[String] =
    val ms =
      (for
        t <- ts
        case m: Type.Meta <- free(zonk(t))
      yield m).distinct
    val su: Map[Type, Type] = ms.zipWithIndex.map((m, i) => m -> Type.Meta(i + 1)).toMap
    ts.map(t => show(subst(zonk(t), su)))

  def instance(params: List[Type]): Map[Type, Type] =
    params.map(_ -> fresh()).toMap

  def instantiate(s: (List[Type], Type)): Type =
    val (params, typ) = s
    subst(zonk(typ), instance(params))

  // local variables hide functions, constructors, and builtins
  def lookup(env: Map[Name, (List[Type], Type)], x: Name): List[(List[Type], Type)] =
    if env.contains(x) then
      List(env(x))
    else
      funs.getOrElse(x, Nil) ++ constrs.get(x).map(scheme) ++ builtins.get(x)

  // the meaning of x applied to n arguments, or n < 0 if x is not applied
  def resolve(env: Map[Name, (List[Type], Type)], x: Name, n: Int): (List[Type], Type) =
    lookup(env, x) match
      case Nil =>
        error("type", s"unknown variable $x")
      case List(s) =>
        s
      case ss if n < 0 =>
        error("type", s"overloaded $x must be applied")
      case ss =>
        ss.find((_, t) => arity(t) == n).getOrElse(error("type", s"$x does not take $n arguments"))

  // a use of x elaborated with an instance of s
  def use(x: Name, t: Type, s: (List[Type], Type), pos: Pos): Expr.Var =
    val u = instantiate(s)
    unify(u, normalize(t))
    Expr.Var(x, u)(pos)

  def generalize(env: Map[Name, (List[Type], Type)], t: Type): (List[Type], Type) =
    val fixed = env.values.flatMap((params, u) => free(zonk(u)).diff(params)).toSet
    val typ = zonk(t)
    (
      for
        case m: Type.Meta <- free(typ)
        if !fixed(m)
      yield m,
      typ
    )

  def infer(env: Map[Name, (List[Type], Type)], e: Expr): (Expr, Type) =
    at(e.pos):
      e match
        case Expr.Lit(_: Boolean) =>
          (e, Bool)
        case Expr.Lit(_: BigInt) =>
          (e, Int)
        case Expr.Lit(_: String) =>
          (e, Str)
        case Expr.Lit(_: Char) =>
          (e, Char)
        case Expr.Lit(value) =>
          error("type", s"invalid literal $value")

        case Expr.Var(name, typ) =>
          val v = use(name, typ, resolve(env, name, -1), e.pos)
          (v, v.typ)

        case Expr.App(Expr.Var(name, typ), args) if lookup(env, name).length > 1 =>
          val v = use(name, typ, resolve(env, name, args.length), e.pos)
          val (args1, r) = apply(env, v.typ, args)
          (Expr.App(v, args1)(e.pos), r)

        case Expr.App(fun, args) =>
          val (fun1, t) = infer(env, fun)
          val (args1, r) = apply(env, t, args)
          (Expr.App(fun1, args1)(e.pos), r)

        case Expr.Lam(bound, body) =>
          val xs = formals(bound)
          val (body1, t) = infer(env ++ bind(xs), body)
          (Expr.Lam(xs, body1)(e.pos), fun(xs.types, t))

        case Expr.Let(bound, body) =>
          val (vars, args) = bound.unzip
          val xs = formals(vars)
          val args1 = check(env, args, xs.types)
          val ss = xs.map(x => x.name -> generalize(env, x.typ))
          val (body1, t) = infer(env ++ ss, body)
          (Expr.Let(xs.zip(args1), body1)(e.pos), t)

        case Expr.Ite(test, left, right) =>
          val test1 = check(env, test, Bool)
          val (left1, t) = infer(env, left)
          val right1 = check(env, right, t)
          (Expr.Ite(test1, left1, right1)(e.pos), t)

        case Expr.Comp(result, bound, lists, test) =>
          val xs = formals(bound)
          sameLength(xs, lists, "comprehension")
          val lists1 = generators(env, xs, lists)
          val test1 = check(env ++ bind(xs), test, Bool)
          val (result1, t) = infer(env ++ bind(xs), result)
          (Expr.Comp(result1, xs, lists1, test1)(e.pos), Type.Sort("List", List(t)))

        case Expr.Match(arg, cases) =>
          val r = fresh()
          val (arg1, cases1) = branches(env, arg, cases)((bs, b) => check(env ++ bs, b, r))
          (Expr.Match(arg1, cases1)(e.pos), r)

        case Expr.Tuple(items) =>
          val (items1, ts) = infer(env, items)
          (Expr.Tuple(items1)(e.pos), Type.Tuple(ts))

  // each list may refer to the variables bound before it
  def generators(env: Map[Name, (List[Type], Type)], xs: List[Pat.Var], lists: List[Expr]): List[Expr] =
    (xs, lists) match
      case (Nil, Nil) =>
        Nil
      case (x :: xs, list :: lists) =>
        check(env, list, Type.Sort("List", List(x.typ))) :: generators(env ++ bind(List(x)), xs, lists)
      case _ =>
        error("type", s"expected ${xs.length} lists, got ${lists.length}")

  // checks the patterns against arg and each body with the variables its pattern binds
  def branches[A](env: Map[Name, (List[Type], Type)], arg: Expr, cases: List[Case[A]])(elaborate: (Map[Name, (List[Type], Type)], A) => A): (Expr, List[Case[A]]) =
    val (arg1, t) = infer(env, arg)
    val cases1 =
      for Case(pat, guard, body) <- cases
      yield
        val (pat1, bs) = pattern(pat, t)
        Case(pat1, guard.map(check(env ++ bs, _, Bool)), elaborate(bs, body))
    reachable(t, cases1)
    exhaustive(t, cases1)
    (arg1, cases1)

  def pattern(p: Pat, t: Type): (Pat, Map[Name, (List[Type], Type)]) =
    val (p1, bs) = binders(p, t)
    distinct(bs.names, "pattern variable")
    (p1, bind(bs))

  def binders(p: Pat, t: Type): (Pat, List[Pat.Var]) =
    at(p.pos):
      p match
        case Pat.Wild() =>
          (p, Nil)

        case Pat.Lit(value) =>
          check(Map(), Expr.Lit(value)(p.pos), t)
          (p, Nil)

        case Pat.Var(name, _) if constrs.contains(name) =>
          binders(Pat.App(name, Nil)(p.pos), t)

        case Pat.Var(name, typ) =>
          val u = normalize(typ)
          unify(u, t)
          val x: Pat.Var = Pat.Var(name, u)(p.pos)
          (x, List(x))

        case Pat.App(name, args) =>
          val (params, kargs, res) = constrs.getOrElse(name, error("type", s"unknown constructor $name"))
          sameLength(kargs, args, s"constructor $name")
          val su = instance(params)
          unify(subst(res, su), t)
          val (args1, bs) = binders(args, kargs.map(subst(_, su)))
          (Pat.App(name, args1)(p.pos), bs)

        case Pat.Tuple(items) =>
          val ts = items.map(_ => fresh())
          unify(Type.Tuple(ts), t)
          val (items1, bs) = binders(items, ts)
          (Pat.Tuple(items1)(p.pos), bs)

  def binders(ps: List[Pat], ts: List[Type]): (List[Pat], List[Pat.Var]) =
    (ps, ts) match
      case (Nil, Nil) =>
        (Nil, Nil)
      case (p :: ps, t :: ts) =>
        val (p1, bs) = binders(p, t)
        val (ps1, cs) = binders(ps, ts)
        (p1 :: ps1, bs ++ cs)
      case _ =>
        error("type", s"expected ${ts.length} patterns, got ${ps.length}")

  // the patterns of cases without guards, since guarded cases may not apply
  def unguarded(cases: List[Case[?]]): List[List[Pat]] =
    for
      c <- cases
      if c.guard.isEmpty
    yield List(c.pat)

  def exhaustive(t: Type, cases: List[Case[?]]): Unit =
    for w <- missing(unguarded(cases), List(t)) do
      error("type", s"match is not exhaustive, missing ${w.head}")

  def reachable(t: Type, cases: List[Case[?]]): Unit =
    for (c, i) <- cases.zipWithIndex do
      at(c.pat.pos)(require("type", useful(unguarded(cases.take(i)), List(c.pat), List(t)), s"unreachable case ${Printer.show(c.pat)}"))

  // the constructors in the first column
  def heads(rows: List[List[Pat]]): Set[Name] =
    rows.flatMap(r => head(r.head)).map(_._1).toSet

  // the type has finitely many constructors, which all occur in the first column
  def complete(rows: List[List[Pat]], t: Type): Boolean =
    signature(t).exists(_.forall((c, _) => heads(rows)(c)))

  // the argument types of constructor c of type t
  def fields(t: Type, c: Name): List[Type] =
    for
      cs <- signature(t).toList
      (d, args) <- cs
      if d == c
      a <- args
    yield a

  // some values of types ts match q but no row
  def useful(rows: List[List[Pat]], q: List[Pat], ts: List[Type]): Boolean =
    (q, ts) match
      case (Nil, _) =>
        rows.isEmpty
      case (p :: ps, t :: rest) if head(p).nonEmpty =>
        val (c, args) = head(p).get
        useful(specialize(rows, c, args.length), args ++ ps, fields(t, c) ++ rest)
      case (_ :: ps, t :: rest) if complete(rows, t) =>
        signature(t).get
          .exists((c, args) => useful(specialize(rows, c, args.length), args.map(_ => Pat.Wild()(nowhere)) ++ ps, args ++ rest))
      case (_ :: ps, _ :: rest) =>
        useful(rows.filter(r => head(r.head).isEmpty).map(_.tail), ps, rest)
      case (_, Nil) =>
        false

  // the constructor of an elaborated pattern, literals count as constructors without arguments
  // the constructor of tuples is ,
  def head(p: Pat): Option[(Name, List[Pat])] =
    p match
      case Pat.App(name, args) =>
        Some(name -> args)
      case Pat.Tuple(items) =>
        Some("," -> items)
      case Pat.Lit(value) =>
        Some(Printer.show(value) -> Nil)
      case _ =>
        None

  def witness(c: Name, args: List[String]): String =
    (c, args) match
      case (",", _) =>
        args.mkString("(", ", ", ")")
      case ("Cons", List(x, xs)) =>
        s"[$x; $xs]"
      case (_, List(l, r)) if Printer.symbolic(c) =>
        Printer.infix(c, l, r)
      case (_, Nil) =>
        c
      case _ =>
        args.mkString(s"$c(", ", ", ")")

  // all constructors of t with their argument types, if there are finitely many
  def signature(t: Type): Option[List[(Name, List[Type])]] =
    zonk(t) match
      case Type.Sort("Bool", Nil) =>
        Some(List("true" -> Nil, "false" -> Nil))
      case Type.Tuple(ts) =>
        Some(List("," -> ts))
      case Type.Sort(name, args) if datas.contains(name) =>
        Some(
          for c <- members(name)
          yield
            val (params, kargs, _) = constrs(c)
            c -> kargs.map(subst(_, params.zip(args).toMap))
        )
      case _ =>
        None

  // values of types ts, shown as patterns, that no row matches
  def missing(rows: List[List[Pat]], ts: List[Type]): Option[List[String]] =
    ts match
      case Nil =>
        Option.when(rows.isEmpty)(Nil)
      case t :: rest =>
        missing(rows, heads(rows), signature(t), rest)

  // heads are the constructors in the first column, cs are all constructors of its type if there are finitely many
  def missing(rows: List[List[Pat]], heads: Set[Name], cs: Option[List[(Name, List[Type])]], rest: List[Type]): Option[List[String]] =
    cs match
      case Some(cs) if cs.forall((c, _) => heads(c)) =>
        first(rows, cs, rest)
      case _ =>
        missing(rows.filter(r => head(r.head).isEmpty).map(_.tail), rest).map(uncovered(heads, cs) :: _)

  // the first missing value whose constructor is one of cs
  def first(rows: List[List[Pat]], cs: List[(Name, List[Type])], rest: List[Type]): Option[List[String]] =
    cs match
      case Nil =>
        None
      case (c, args) :: cs =>
        missing(specialize(rows, c, args.length), args ++ rest) match
          case Some(ws) =>
            Some(witness(c, ws.take(args.length)) :: ws.drop(args.length))
          case None =>
            first(rows, cs, rest)

  // a value of the first column that heads do not cover
  def uncovered(heads: Set[Name], cs: Option[List[(Name, List[Type])]]): String =
    val ws =
      for
        cs <- cs.toList
        (c, args) <- cs
        if heads.nonEmpty && !heads(c)
      yield witness(c, args.map(_ => "_"))
    ws match
      case w :: _ =>
        w
      case Nil =>
        "_"

  def specialize(rows: List[List[Pat]], c: Name, n: Int): List[List[Pat]] =
    for
      r <- rows
      if head(r.head).forall(_._1 == c)
    yield head(r.head) match
      case Some((_, ps)) =>
        ps ++ r.tail
      case None =>
        List.fill(n)(Pat.Wild()(nowhere)) ++ r.tail

  def check(env: Map[Name, (List[Type], Type)], e: Expr, t: Type): Expr =
    val (e1, t1) = infer(env, e)
    at(e.pos)(unify(t1, t))
    e1

  def check(env: Map[Name, (List[Type], Type)], es: List[Expr], ts: List[Type]): List[Expr] =
    (es, ts) match
      case (Nil, Nil) =>
        Nil
      case (e :: es, t :: ts) =>
        check(env, e, t) :: check(env, es, ts)
      case _ =>
        error("type", s"expected ${ts.length} values, got ${es.length}")

  def infer(env: Map[Name, (List[Type], Type)], es: List[Expr]): (List[Expr], List[Type]) =
    es.map(infer(env, _)).unzip

  def apply(env: Map[Name, (List[Type], Type)], f: Type, args: List[Expr]): (List[Expr], Type) =
    val r = fresh()
    val (args1, ts) = infer(env, args)
    unify(f, fun(ts, r))
    (args1, r)

  def variable(env: Map[Name, (List[Type], Type)], assignable: Set[Name], x: Expr.Var): Expr.Var =
    at(x.pos):
      require("type", assignable(x.name), s"${x.name} is not assignable")
      val (_, t) = env(x.name)
      unify(t, normalize(x.typ))
      Expr.Var(x.name, t)(x.pos)

  def stmt(env: Map[Name, (List[Type], Type)], assignable: Set[Name], s: Stmt): Stmt =
    at(s.pos):
      s match
        case Stmt.Skip() =>
          s

        case Stmt.Assign(pairs) =>
          val (vars, args) = pairs.unzip
          distinct(vars.names, "assignment to")
          val vars1 = vars.map(variable(env, assignable, _))
          Stmt.Assign(vars1.zip(check(env, args, vars1.types)))(s.pos)

        case Stmt.Block(locals, stmts) =>
          val xs = signature(locals)
          Stmt.Block(xs, stmts.map(stmt(env ++ bind(xs), assignable ++ xs.names, _)))(s.pos)

        case Stmt.If(test, left, right) =>
          Stmt.If(check(env, test, Bool), stmt(env, assignable, left), stmt(env, assignable, right))(s.pos)

        case Stmt.While(test, body) =>
          Stmt.While(check(env, test, Bool), stmt(env, assignable, body))(s.pos)

        case Stmt.Call(proc, ins, outs, refs) =>
          val (params, is, os, rs) = procedure(proc).getOrElse(error("type", s"unknown procedure $proc"))
          val su = instance(params)
          sameLength(is, ins, proc)
          sameLength(os, outs, proc)
          sameLength(rs, refs, proc)
          distinct((outs ++ refs).names, s"argument to $proc")
          val ins1 = check(env, ins, is.map(subst(_, su)))
          val outs1 = outs.map(variable(env, assignable, _))
          val refs1 = refs.map(variable(env, assignable, _))
          unify((outs1 ++ refs1).types, (os ++ rs).map(subst(_, su)))
          Stmt.Call(proc, ins1, outs1, refs1)(s.pos)

        case Stmt.Match(arg, cases) =>
          val (arg1, cases1) = branches(env, arg, cases)((bs, b) => stmt(env ++ bs, assignable -- bs.keys, b))
          Stmt.Match(arg1, cases1)(s.pos)

        case Stmt.Assert(test) =>
          Stmt.Assert(check(env, test, Bool))(s.pos)

        case Stmt.Raise(arg, typ) =>
          val (arg1, t) = infer(env, arg)
          unify(t, normalize(typ))
          Stmt.Raise(arg1, t)(s.pos)

        case Stmt.Try(body, handlers) =>
          val handlers1 =
            for Handler(typ, pat, guard, b) <- handlers
            yield
              val t = normalize(typ)
              val (pat1, bs) = pattern(pat, t)
              Handler(t, pat1, guard.map(check(env ++ bs, _, Bool)), stmt(env ++ bs, assignable -- bs.keys, b))
          Stmt.Try(stmt(env, assignable, body), handlers1)(s.pos)

  // the procedures of the program, which hide the builtin ones
  def procedure(name: Name): Option[(List[Type], List[Type], List[Type], List[Type])] =
    procs.get(name) match
      case Some(sig) =>
        Some(sig)
      case None =>
        builtinProcedure(name)

  // read and library give the syntax of self/syntax.u, so they exist only in programs that declare its Program
  def builtinProcedure(name: Name): Option[(List[Type], List[Type], List[Type], List[Type])] =
    name match
      case "read" | "library" if datas.contains("Program") =>
        Some((Nil, List(Str), List(Type.Sort("Program", Nil)), Nil))
      case "write" =>
        Some((Nil, List(Str, Str), Nil, Nil))
      case _ =>
        None

  def declare(name: Name): Unit =
    require(
      "type",
      !funs.contains(name) && !procs.contains(name) && !constrs.contains(name) && !builtins.contains(name),
      s"$name is already defined"
    )

  // functions of the same name must differ in their number of arguments
  def overload(name: Name, n: Int): Unit =
    require(
      "type",
      !procs.contains(name) && !constrs.contains(name) && !lookup(Map(), name).exists((_, t) => arity(t) == n),
      s"$name is already defined"
    )

  def declareSort(name: Name): Unit =
    require("type", !basic(name) && name != "->" && !aliases.contains(name) && !datas.contains(name), s"sort $name is already defined")

  def cmds(): Program =
    // datatypes may be mutually recursive, so they are known before any sort is checked
    for case d @ Cmd.Data(name, params, _) <- program.cmds do
      at(d.pos):
        declareSort(name)
        update(c => c.copy(datas = c.datas + (name -> params.length)))

    def closed(what: String, params: List[Name], t: Type): Unit =
      val undeclared = free(t).diff(params.map(Type.Param(_)))
      require("type", undeclared.isEmpty, s"$what has undeclared type parameters ${undeclared.map(show(_)).mkString(", ")}")

    for case d @ Cmd.Sort(name, params, typ) <- program.cmds do
      at(d.pos):
        declareSort(name)
        distinct(params, "type parameter")
        val rhs = known(typ, s"sort $name")
        closed(s"sort $name", params, rhs)
        update(c => c.copy(aliases = c.aliases + (name -> (params, rhs))))

    for case d @ Cmd.Data(name, params, cs) <- program.cmds do
      at(d.pos):
        distinct(params, "type parameter")
        val ps = params.map(Type.Param(_))
        update(c => c.copy(members = c.members + (name -> cs.map(_.name))))
        for Constr(c, args) <- cs do
          declare(c)
          val ts = args.map(known(_, s"constructor $c"))
          for t <- ts do
            closed(s"sort $name", params, t)
          update(ctx => ctx.copy(constrs = ctx.constrs + (c -> (ps, ts, Type.Sort(name, ps)))))

    for case d @ Cmd.Fun(name, params, bound, res, _) <- program.cmds do
      at(d.pos):
        distinct(params, "type parameter")
        overload(name, bound.head.length)
        distinct(bound.flatten.names, "variable")
        val t = curried(bound.map(signature(_).types), known(res, s"result of $name"))
        closed(name, params, t)
        update(c => c.copy(funs = c.funs + (name -> (c.funs.getOrElse(name, Nil) :+ (params.map(Type.Param(_)), t)))))

    for case d @ Cmd.Proc(name, params, ins, outs, refs, _) <- program.cmds do
      at(d.pos):
        declare(name)
        distinct(params, "type parameter")
        val ts = signature(ins ++ outs ++ refs).types
        for t <- ts do
          closed(name, params, t)
        val (is, rest) = ts.splitAt(ins.length)
        val (os, rs) = rest.splitAt(outs.length)
        update(c => c.copy(procs = c.procs + (name -> (params.map(Type.Param(_)), is, os, rs))))

    distinct(
      program.cmds.collect { case Cmd.Test(name, _) =>
        name
      },
      "test"
    )
    Program(program.cmds.map(c => at(c.pos)(zonk(command(c)))))

  // the elaborated command c
  def command(c: Cmd): Cmd =
    c match
      case d: Cmd.Sort =>
        d
      case d: Cmd.Data =>
        d

      case Cmd.Fun(name, params, bound, res, body) =>
        val xss = bound.map(signature)
        val t = known(res, s"result of $name")
        Cmd.Fun(name, params, xss, t, check(bind(xss.flatten), body, t))(c.pos)

      case Cmd.Proc(name, params, ins, outs, refs, body) =>
        val is = signature(ins)
        val os = signature(outs)
        val rs = signature(refs)
        Cmd.Proc(name, params, is, os, rs, stmt(bind(is ++ os ++ rs), (os ++ rs).names.toSet, body))(c.pos)

      case d: Cmd.Import =>
        d

      case d: Cmd.Notation =>
        d

      case Cmd.Eval(expr) =>
        Cmd.Eval(infer(Map(), expr)._1)(c.pos)

      case Cmd.Run(body) =>
        Cmd.Run(stmt(Map(), Set(), body))(c.pos)

      case Cmd.Test(name, body) =>
        Cmd.Test(name, stmt(Map(), Set(), body))(c.pos)
