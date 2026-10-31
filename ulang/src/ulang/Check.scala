package ulang

// params are the Params of a generic declaration or the Metas generalized by a let
case class Scheme(params: List[Type], typ: Type)

case class Sig(params: List[Type], ins: List[Type], outs: List[Type], refs: List[Type])

case class Ctx(
    aliases: Map[Name, (List[Name], Type)] = Map(),
    funs: Map[Name, List[Scheme]] = Map(),
    procs: Map[Name, Sig] = Map(),
    datas: Map[Name, Int] = Map(),
    members: Map[Name, List[Name]] = Map(),
    constrs: Map[Name, Constructor] = Map()
)

case class Constructor(params: List[Type], args: List[Type], res: Type):
  def scheme: Scheme =
    Scheme(
      params,
      if args.isEmpty then
        res
      else
        Check.fun(args, res)
    )

object Check:
  val Bool = Type.Sort("Bool", Nil)
  val Int = Type.Sort("Int", Nil)
  val Str = Type.Sort("Str", Nil)
  val Unit = Type.Sort("Unit", Nil)

  val basic = Set("Bool", "Int", "Str", "Unit")

  def fun(args: List[Type], res: Type): Type =
    Type.Sort("->", args :+ res)

  def mono(t: Type): Scheme =
    Scheme(Nil, t)

  // pairs are a datatype *[A, B] with the constructor ,
  val initial: Ctx =
    val ps = List(Type.Param("A"), Type.Param("B"))
    Ctx(datas = Map("*" -> 2), members = Map("*" -> List(",")), constrs = Map("," -> Constructor(ps, ps, Type.Sort("*", ps))))

  // only what cannot be defined in the prelude
  val builtins: Map[Name, Scheme] =
    val arith = mono(fun(List(Int, Int), Int))
    val logic = mono(fun(List(Bool, Bool), Bool))
    val eq = Scheme(List(Type.Param("A")), fun(List(Type.Param("A"), Type.Param("A")), Bool))
    Map(
      "+" -> arith,
      "-" -> arith,
      "*" -> arith,
      "/" -> arith,
      "%" -> arith,
      "<" -> mono(fun(List(Int, Int), Bool)),
      "==" -> eq,
      "and" -> logic,
      "or" -> logic
    )

  // the number of arguments of a function, functions are overloaded by it
  def arity(t: Type): Int =
    t match
      case Type.Sort("->", args) =>
        args.length - 1
      case _ =>
        -1

  def show(t: Type): String =
    t match
      case Type.Param(name) =>
        name
      case Type.Hole =>
        "_"
      case Type.Meta(index) =>
        s"?$index"
      case Type.Sort("->", args) =>
        args.init.map(show(_)).mkString("(", ", ", ")") + " -> " + show(args.last)
      case Type.Sort("*", args) =>
        args.map(show(_)).mkString("(", ", ", ")")
      case Type.Sort(name, Nil) =>
        name
      case Type.Sort(name, args) =>
        args.map(show(_)).mkString(s"$name[", ", ", "]")

  // the Params and Metas in t
  def free(t: Type): List[Type] =
    t match
      case Type.Param(_) | Type.Meta(_) =>
        List(t)
      case Type.Hole =>
        Nil
      case Type.Sort(_, args) =>
        args.flatMap(free).distinct

  def holes(t: Type): Boolean =
    t match
      case Type.Hole =>
        true
      case Type.Sort(_, args) =>
        args.exists(holes)
      case _ =>
        false

  // constructors named by operators are shown infix
  def symbolic(name: Name): Boolean =
    !name.head.isLetter && name.head != '_' && name != ","

  def infix(op: Name, l: String, r: String): String =
    def operand(a: String): String =
      if a.contains(" ") && !a.startsWith("(") then
        s"($a)"
      else
        a
    s"${operand(l)} $op ${operand(r)}"

  def show(p: Pat): String =
    p match
      case Pat.Wild =>
        "_"
      case Pat.Lit(value) =>
        Printer.show(value)
      case Pat.Var(name, _) =>
        name
      case Pat.App(",", args) =>
        args.map(show(_)).mkString("(", ", ", ")")
      case Pat.App(name, List(l, r)) if symbolic(name) =>
        infix(name, show(l), show(r))
      case Pat.App(name, Nil) =>
        name
      case Pat.App(name, args) =>
        args.map(show(_)).mkString(s"$name(", ", ", ")")

  def subst(t: Type, su: Map[Type, Type]): Type =
    t match
      case Type.Sort(name, args) =>
        Type.Sort(name, args.map(subst(_, su)))
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
      case Type.Sort("->", args) if args.nonEmpty =>
        Type.Sort("->", args.map(normalize))
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
    xs.map(x => Pat.Var(x.name, normalize(x.typ)))

  def signature(xs: List[Pat.Var]): List[Pat.Var] =
    distinct(xs.names, "variable")
    xs.map(x => Pat.Var(x.name, known(x.typ, x.name)))

  def bind(xs: List[Pat.Var]): Map[Name, Scheme] =
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
      case t =>
        t

  def zonk(x: Expr.Var): Expr.Var =
    Expr.Var(x.name, zonk(x.typ))

  def zonk(x: Pat.Var): Pat.Var =
    Pat.Var(x.name, zonk(x.typ))

  def zonk(e: Expr): Expr =
    e match
      case Expr.Lit(_) =>
        e
      case x: Expr.Var =>
        zonk(x)
      case Expr.App(fun, args) =>
        Expr.App(zonk(fun), args.map(zonk(_)))
      case Expr.Lam(bound, body) =>
        Expr.Lam(bound.map(zonk(_)), zonk(body))
      case Expr.Let(bound, args, body) =>
        Expr.Let(bound.map(zonk(_)), args.map(zonk(_)), zonk(body))
      case Expr.Ite(test, left, right) =>
        Expr.Ite(zonk(test), zonk(left), zonk(right))
      case Expr.Match(arg, cases) =>
        Expr.Match(zonk(arg), cases.map(c => Case(zonk(c.pat), zonk(c.body))))
      case Expr.Comp(result, bound, lists, test) =>
        Expr.Comp(zonk(result), bound.map(zonk(_)), lists.map(zonk(_)), zonk(test))

  def zonk(p: Pat): Pat =
    p match
      case Pat.Wild | Pat.Lit(_) =>
        p
      case Pat.Var(name, typ) =>
        Pat.Var(name, zonk(typ))
      case Pat.App(name, args) =>
        Pat.App(name, args.map(zonk(_)))

  def zonk(s: Stmt): Stmt =
    s match
      case Stmt.Skip =>
        s
      case Stmt.Assign(vars, args) =>
        Stmt.Assign(vars.map(zonk(_)), args.map(zonk(_)))
      case Stmt.Block(locals, stmts) =>
        Stmt.Block(locals.map(zonk(_)), stmts.map(zonk(_)))
      case Stmt.If(test, left, right) =>
        Stmt.If(zonk(test), zonk(left), zonk(right))
      case Stmt.While(test, body) =>
        Stmt.While(zonk(test), zonk(body))
      case Stmt.Call(proc, ins, outs, refs) =>
        Stmt.Call(proc, ins.map(zonk(_)), outs.map(zonk(_)), refs.map(zonk(_)))
      case Stmt.Match(arg, cases) =>
        Stmt.Match(zonk(arg), cases.map(c => Case(zonk(c.pat), zonk(c.body))))
      case Stmt.Assert(test) =>
        Stmt.Assert(zonk(test))

  def zonk(d: Cmd): Cmd =
    d match
      case Cmd.Sort(_, _, _) | Cmd.Data(_, _, _) =>
        d
      case Cmd.Fun(name, params, bound, res, body) =>
        Cmd.Fun(name, params, bound.map(zonk(_)), zonk(res), zonk(body))
      case Cmd.Proc(name, params, ins, outs, refs, body) =>
        Cmd.Proc(name, params, ins.map(zonk(_)), outs.map(zonk(_)), refs.map(zonk(_)), zonk(body))
      case Cmd.Import(_) | Cmd.Notation(_, _, _) =>
        d
      case Cmd.Eval(expr) =>
        Cmd.Eval(zonk(expr))
      case Cmd.Run(body) =>
        Cmd.Run(zonk(body))
      case Cmd.Test(body) =>
        Cmd.Test(zonk(body))

  def occurs(i: Int, t: Type): Boolean =
    resolve(t) match
      case Type.Meta(index) =>
        i == index
      case Type.Sort(_, args) =>
        args.exists(occurs(i, _))
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
    val ms = ts.map(zonk(_)).flatMap(free).filter(_.isInstanceOf[Type.Meta]).distinct
    val su: Map[Type, Type] = ms.zipWithIndex.map((m, i) => m -> Type.Meta(i + 1)).toMap
    ts.map(t => show(subst(zonk(t), su)))

  def instance(params: List[Type]): Map[Type, Type] =
    params.map(_ -> fresh()).toMap

  def instantiate(s: Scheme): Type =
    subst(zonk(s.typ), instance(s.params))

  // local variables hide functions, constructors, and builtins
  def lookup(env: Map[Name, Scheme], x: Name): List[Scheme] =
    if env.contains(x) then
      List(env(x))
    else
      funs.getOrElse(x, Nil) ++ constrs.get(x).map(_.scheme) ++ builtins.get(x)

  // the meaning of x applied to n arguments, or n < 0 if x is not applied
  def resolve(env: Map[Name, Scheme], x: Name, n: Int): Scheme =
    lookup(env, x) match
      case Nil =>
        error("type", s"unknown variable $x")
      case List(s) =>
        s
      case ss if n < 0 =>
        error("type", s"overloaded $x must be applied")
      case ss =>
        ss.find(s => arity(s.typ) == n).getOrElse(error("type", s"$x does not take $n arguments"))

  // a use of x elaborated with an instance of s
  def use(x: Name, t: Type, s: Scheme): Expr.Var =
    val u = instantiate(s)
    unify(u, normalize(t))
    Expr.Var(x, u)

  def generalize(env: Map[Name, Scheme], t: Type): Scheme =
    val fixed = env.values.flatMap(s => free(zonk(s.typ)).diff(s.params)).toSet
    val typ = zonk(t)
    Scheme(free(typ).filter(p => p.isInstanceOf[Type.Meta] && !fixed(p)), typ)

  def infer(env: Map[Name, Scheme], e: Expr): (Expr, Type) =
    e match
      case Expr.Lit(()) =>
        (e, Unit)
      case Expr.Lit(_: Boolean) =>
        (e, Bool)
      case Expr.Lit(_: BigInt) =>
        (e, Int)
      case Expr.Lit(_: String) =>
        (e, Str)
      case Expr.Lit(value) =>
        error("type", s"invalid literal $value")

      case Expr.Var(name, typ) =>
        val v = use(name, typ, resolve(env, name, -1))
        (v, v.typ)

      case Expr.App(Expr.Var(name, typ), args) if lookup(env, name).length > 1 =>
        val v = use(name, typ, resolve(env, name, args.length))
        val (args1, r) = apply(env, v.typ, args)
        (Expr.App(v, args1), r)

      case Expr.App(fun, args) =>
        val (fun1, t) = infer(env, fun)
        val (args1, r) = apply(env, t, args)
        (Expr.App(fun1, args1), r)

      case Expr.Lam(bound, body) =>
        val xs = formals(bound)
        val (body1, t) = infer(env ++ bind(xs), body)
        (Expr.Lam(xs, body1), fun(xs.types, t))

      case Expr.Let(bound, args, body) =>
        val xs = formals(bound)
        sameLength(xs, args, "let")
        val args1 = check(env, args, xs.types)
        val ss = xs.map(x => x.name -> generalize(env, x.typ))
        val (body1, t) = infer(env ++ ss, body)
        (Expr.Let(xs, args1, body1), t)

      case Expr.Ite(test, left, right) =>
        val test1 = check(env, test, Bool)
        val (left1, t) = infer(env, left)
        val right1 = check(env, right, t)
        (Expr.Ite(test1, left1, right1), t)

      case Expr.Comp(result, bound, lists, test) =>
        val xs = formals(bound)
        sameLength(xs, lists, "comprehension")
        val lists1 = generators(env, xs, lists)
        val test1 = check(env ++ bind(xs), test, Bool)
        val (result1, t) = infer(env ++ bind(xs), result)
        (Expr.Comp(result1, xs, lists1, test1), Type.Sort("List", List(t)))

      case Expr.Match(arg, cases) =>
        val r = fresh()
        val (arg1, cases1) = branches(env, arg, cases)((bs, b) => check(env ++ bs, b, r))
        (Expr.Match(arg1, cases1), r)

  // each list may refer to the variables bound before it
  def generators(env: Map[Name, Scheme], xs: List[Pat.Var], lists: List[Expr]): List[Expr] =
    (xs, lists) match
      case (Nil, Nil) =>
        Nil
      case (x :: xs, list :: lists) =>
        check(env, list, Type.Sort("List", List(x.typ))) :: generators(env ++ bind(List(x)), xs, lists)
      case _ =>
        error("type", s"expected ${xs.length} lists, got ${lists.length}")

  // checks the patterns against arg and each body with the variables its pattern binds
  def branches[A](env: Map[Name, Scheme], arg: Expr, cases: List[Case[A]])(elaborate: (Map[Name, Scheme], A) => A): (Expr, List[Case[A]]) =
    val (arg1, t) = infer(env, arg)
    val cases1 =
      for Case(pat, body) <- cases
      yield
        val (pat1, bs) = pattern(pat, t)
        Case(pat1, elaborate(bs, body))
    reachable(t, cases1)
    exhaustive(t, cases1)
    (arg1, cases1)

  def pattern(p: Pat, t: Type): (Pat, Map[Name, Scheme]) =
    val (p1, bs) = binders(p, t)
    distinct(bs.names, "pattern variable")
    (p1, bind(bs))

  def binders(p: Pat, t: Type): (Pat, List[Pat.Var]) =
    p match
      case Pat.Wild =>
        (p, Nil)

      case Pat.Lit(value) =>
        check(Map(), Expr.Lit(value), t)
        (p, Nil)

      case Pat.Var(name, _) if constrs.contains(name) =>
        binders(Pat.App(name, Nil), t)

      case Pat.Var(name, typ) =>
        val u = normalize(typ)
        unify(u, t)
        val x: Pat.Var = Pat.Var(name, u)
        (x, List(x))

      case Pat.App(name, args) =>
        val k = constrs.getOrElse(name, error("type", s"unknown constructor $name"))
        sameLength(k.args, args, s"constructor $name")
        val su = instance(k.params)
        unify(subst(k.res, su), t)
        val (args1, bs) = binders(args, k.args.map(subst(_, su)))
        (Pat.App(name, args1), bs)

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

  def exhaustive(t: Type, cases: List[Case[?]]): Unit =
    for w <- missing(cases.map(c => List(c.pat)), List(t)) do
      error("type", s"match is not exhaustive, missing ${w.head}")

  def reachable(t: Type, cases: List[Case[?]]): Unit =
    for (c, i) <- cases.zipWithIndex do
      require("type", useful(cases.take(i).map(c => List(c.pat)), List(c.pat), List(t)), s"unreachable case ${show(c.pat)}")

  // the constructors in the first column
  def heads(rows: List[List[Pat]]): Set[Name] =
    rows.flatMap(r => head(r.head)).map(_._1).toSet

  // the type has finitely many constructors, which all occur in the first column
  def complete(rows: List[List[Pat]], t: Type): Boolean =
    signature(t).exists(_.forall((c, _) => heads(rows)(c)))

  // the argument types of constructor c of type t
  def fields(t: Type, c: Name): List[Type] =
    signature(t).flatMap(_.find(_._1 == c)).map(_._2).getOrElse(Nil)

  // some values of types ts match q but no row
  def useful(rows: List[List[Pat]], q: List[Pat], ts: List[Type]): Boolean =
    (q, ts) match
      case (Nil, _) =>
        rows.isEmpty
      case (p :: ps, t :: rest) if head(p).nonEmpty =>
        val (c, args) = head(p).get
        useful(specialize(rows, c, args.length), args ++ ps, fields(t, c) ++ rest)
      case (_ :: ps, t :: rest) if complete(rows, t) =>
        signature(t).get.exists((c, args) => useful(specialize(rows, c, args.length), args.map(_ => Pat.Wild) ++ ps, args ++ rest))
      case (_ :: ps, _ :: rest) =>
        useful(rows.filter(r => head(r.head).isEmpty).map(_.tail), ps, rest)
      case (_, Nil) =>
        false

  // the constructor of an elaborated pattern, literals count as constructors without arguments
  def head(p: Pat): Option[(Name, List[Pat])] =
    p match
      case Pat.App(name, args) =>
        Some(name -> args)
      case Pat.Lit(value) =>
        Some(Printer.show(value) -> Nil)
      case _ =>
        None

  def witness(c: Name, args: List[String]): String =
    (c, args) match
      case (",", _) =>
        args.mkString("(", ", ", ")")
      case (_, List(l, r)) if symbolic(c) =>
        infix(c, l, r)
      case (_, Nil) =>
        c
      case _ =>
        args.mkString(s"$c(", ", ", ")")

  // all constructors of t with their argument types, if there are finitely many
  def signature(t: Type): Option[List[(Name, List[Type])]] =
    zonk(t) match
      case Type.Sort("Bool", Nil) =>
        Some(List("true" -> Nil, "false" -> Nil))
      case Type.Sort("Unit", Nil) =>
        Some(List("()" -> Nil))
      case Type.Sort(name, args) if datas.contains(name) =>
        Some(
          for c <- members(name)
          yield
            val k = constrs(c)
            c -> k.args.map(subst(_, k.params.zip(args).toMap))
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
        cs.iterator.flatMap((c, args) =>
          missing(specialize(rows, c, args.length), args ++ rest).map(ws => witness(c, ws.take(args.length)) :: ws.drop(args.length))
        ).nextOption()
      case _ =>
        missing(rows.filter(r => head(r.head).isEmpty).map(_.tail), rest).map(uncovered(heads, cs) :: _)

  // a value of the first column that heads do not cover
  def uncovered(heads: Set[Name], cs: Option[List[(Name, List[Type])]]): String =
    cs.flatMap(_.find((c, _) => !heads(c))) match
      case Some((c, args)) if heads.nonEmpty =>
        witness(c, args.map(_ => "_"))
      case _ =>
        "_"

  def specialize(rows: List[List[Pat]], c: Name, n: Int): List[List[Pat]] =
    for
      r <- rows
      if head(r.head).forall(_._1 == c)
    yield head(r.head) match
      case Some((_, ps)) =>
        ps ++ r.tail
      case None =>
        List.fill(n)(Pat.Wild) ++ r.tail

  def check(env: Map[Name, Scheme], e: Expr, t: Type): Expr =
    val (e1, t1) = infer(env, e)
    unify(t1, t)
    e1

  def check(env: Map[Name, Scheme], es: List[Expr], ts: List[Type]): List[Expr] =
    (es, ts) match
      case (Nil, Nil) =>
        Nil
      case (e :: es, t :: ts) =>
        check(env, e, t) :: check(env, es, ts)
      case _ =>
        error("type", s"expected ${ts.length} values, got ${es.length}")

  def infer(env: Map[Name, Scheme], es: List[Expr]): (List[Expr], List[Type]) =
    es.map(infer(env, _)).unzip

  def apply(env: Map[Name, Scheme], f: Type, args: List[Expr]): (List[Expr], Type) =
    val r = fresh()
    val (args1, ts) = infer(env, args)
    unify(f, fun(ts, r))
    (args1, r)

  def variable(env: Map[Name, Scheme], assignable: Set[Name], x: Expr.Var): Expr.Var =
    require("type", assignable(x.name), s"${x.name} is not assignable")
    val t = env(x.name).typ
    unify(t, normalize(x.typ))
    Expr.Var(x.name, t)

  def stmt(env: Map[Name, Scheme], assignable: Set[Name], s: Stmt): Stmt =
    s match
      case Stmt.Skip =>
        s

      case Stmt.Assign(vars, args) =>
        distinct(vars.names, "assignment to")
        sameLength(vars, args, "assignment")
        val vars1 = vars.map(variable(env, assignable, _))
        Stmt.Assign(vars1, check(env, args, vars1.types))

      case Stmt.Block(locals, stmts) =>
        val xs = signature(locals)
        Stmt.Block(xs, stmts.map(stmt(env ++ bind(xs), assignable ++ xs.names, _)))

      case Stmt.If(test, left, right) =>
        Stmt.If(check(env, test, Bool), stmt(env, assignable, left), stmt(env, assignable, right))

      case Stmt.While(test, body) =>
        Stmt.While(check(env, test, Bool), stmt(env, assignable, body))

      case Stmt.Call(proc, ins, outs, refs) =>
        val sig = procs.getOrElse(proc, error("type", s"unknown procedure $proc"))
        val su = instance(sig.params)
        sameLength(sig.ins, ins, proc)
        sameLength(sig.outs, outs, proc)
        sameLength(sig.refs, refs, proc)
        distinct((outs ++ refs).names, s"argument to $proc")
        val ins1 = check(env, ins, sig.ins.map(subst(_, su)))
        val outs1 = outs.map(variable(env, assignable, _))
        val refs1 = refs.map(variable(env, assignable, _))
        unify((outs1 ++ refs1).types, (sig.outs ++ sig.refs).map(subst(_, su)))
        Stmt.Call(proc, ins1, outs1, refs1)

      case Stmt.Match(arg, cases) =>
        val (arg1, cases1) = branches(env, arg, cases)((bs, b) => stmt(env ++ bs, assignable -- bs.keys, b))
        Stmt.Match(arg1, cases1)

      case Stmt.Assert(test) =>
        Stmt.Assert(check(env, test, Bool))

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
      !procs.contains(name) && !constrs.contains(name) && !(funs.getOrElse(name, Nil) ++ builtins.get(name)).exists(s => arity(s.typ) == n),
      s"$name is already defined"
    )

  def declareSort(name: Name): Unit =
    require("type", !basic(name) && name != "->" && !aliases.contains(name) && !datas.contains(name), s"sort $name is already defined")

  def cmds(): Program =
    // datatypes may be mutually recursive, so they are known before any sort is checked
    for case Cmd.Data(name, params, _) <- program.cmds do
      declareSort(name)
      update(c => c.copy(datas = c.datas + (name -> params.length)))

    def closed(what: String, params: List[Name], t: Type): Unit =
      val undeclared = free(t).diff(params.map(Type.Param(_)))
      require("type", undeclared.isEmpty, s"$what has undeclared type parameters ${undeclared.map(show(_)).mkString(", ")}")

    for case Cmd.Sort(name, params, typ) <- program.cmds do
      declareSort(name)
      distinct(params, "type parameter")
      val rhs = known(typ, s"sort $name")
      closed(s"sort $name", params, rhs)
      update(c => c.copy(aliases = c.aliases + (name -> (params, rhs))))

    for case Cmd.Data(name, params, cs) <- program.cmds do
      distinct(params, "type parameter")
      val ps = params.map(Type.Param(_))
      update(c => c.copy(members = c.members + (name -> cs.map(_.name))))
      for Constr(c, args) <- cs do
        declare(c)
        val ts = args.map(known(_, s"constructor $c"))
        for t <- ts do
          closed(s"sort $name", params, t)
        update(ctx => ctx.copy(constrs = ctx.constrs + (c -> Constructor(ps, ts, Type.Sort(name, ps)))))

    for case Cmd.Fun(name, params, bound, res, _) <- program.cmds do
      overload(name, bound.length)
      distinct(params, "type parameter")
      val t = fun(signature(bound).types, known(res, s"result of $name"))
      closed(name, params, t)
      update(c => c.copy(funs = c.funs + (name -> (c.funs.getOrElse(name, Nil) :+ Scheme(params.map(Type.Param(_)), t)))))

    for case Cmd.Proc(name, params, ins, outs, refs, _) <- program.cmds do
      declare(name)
      distinct(params, "type parameter")
      val ts = signature(ins ++ outs ++ refs).types
      for t <- ts do
        closed(name, params, t)
      val (is, rest) = ts.splitAt(ins.length)
      val (os, rs) = rest.splitAt(outs.length)
      update(c => c.copy(procs = c.procs + (name -> Sig(params.map(Type.Param(_)), is, os, rs))))

    val cmds = program.cmds.map:
      case d: Cmd.Sort =>
        d
      case d: Cmd.Data =>
        d

      case Cmd.Fun(name, params, bound, res, body) =>
        val xs = signature(bound)
        val t = known(res, s"result of $name")
        Cmd.Fun(name, params, xs, t, check(bind(xs), body, t))

      case Cmd.Proc(name, params, ins, outs, refs, body) =>
        val is = signature(ins)
        val os = signature(outs)
        val rs = signature(refs)
        Cmd.Proc(name, params, is, os, rs, stmt(bind(is ++ os ++ rs), (os ++ rs).names.toSet, body))

      case d: Cmd.Import =>
        d

      case d: Cmd.Notation =>
        d

      case Cmd.Eval(expr) =>
        Cmd.Eval(infer(Map(), expr)._1)

      case Cmd.Run(body) =>
        Cmd.Run(stmt(Map(), Set(), body))

      case Cmd.Test(body) =>
        Cmd.Test(stmt(Map(), Set(), body))

    Program(cmds.map(zonk(_)))
