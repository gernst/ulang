package ulang

// params are the Params of a generic declaration or the Metas generalized by a let
case class Scheme(params: List[Type], typ: Type)

case class Sig(params: List[Type], ins: List[Type], outs: List[Type], refs: List[Type])

case class Ctx(
    aliases: Map[Name, (List[Name], Type)] = Map(),
    funs: Map[Name, Scheme] = Map(),
    procs: Map[Name, Sig] = Map(),
    datas: Map[Name, Int] = Map(),
    members: Map[Name, List[Name]] = Map(),
    constrs: Map[Name, Constructor] = Map()
)

case class Constructor(params: List[Type], args: List[Type], res: Type):
  def scheme: Scheme = Scheme(params, if args.isEmpty then res else Check.fun(args, res))

object Check:
  val Bool = Type.Sort("Bool", Nil)
  val Int = Type.Sort("Int", Nil)
  val Str = Type.Sort("Str", Nil)
  val Unit = Type.Sort("Unit", Nil)

  val basic = Set("Bool", "Int", "Str", "Unit")

  def fun(args: List[Type], res: Type): Type = Type.Sort("->", args :+ res)

  def mono(t: Type): Scheme = Scheme(Nil, t)

  // pairs are a datatype *[A, B] with the constructor ,
  val initial: Ctx =
    val ps = List(Type.Param("A"), Type.Param("B"))
    Ctx(datas = Map("*" -> 2), members = Map("*" -> List(",")), constrs = Map("," -> Constructor(ps, ps, Type.Sort("*", ps))))

  val builtins: Map[Name, Scheme] =
    val arith = mono(fun(List(Int, Int), Int))
    val cmp = mono(fun(List(Int, Int), Bool))
    val logic = mono(fun(List(Bool, Bool), Bool))
    val eq = Scheme(List(Type.Param("A")), fun(List(Type.Param("A"), Type.Param("A")), Bool))
    Map(
      "+" -> arith,
      "-" -> arith,
      "uminus" -> mono(fun(List(Int), Int)),
      "*" -> arith,
      "/" -> arith,
      "%" -> arith,
      "<" -> cmp,
      "<=" -> cmp,
      ">" -> cmp,
      ">=" -> cmp,
      "==" -> eq,
      "!=" -> eq,
      "and" -> logic,
      "or" -> logic,
      "not" -> mono(fun(List(Bool), Bool))
    )


  def show(t: Type): String = t match
    case Type.Param(name)      => name
    case Type.Hole             => "_"
    case Type.Meta(index)      => s"?$index"
    case Type.Sort("->", args) => args.init.map(show).mkString("(", ", ", ")") + " -> " + show(args.last)
    case Type.Sort("*", args)  => args.map(show).mkString("(", ", ", ")")
    case Type.Sort(name, Nil)  => name
    case Type.Sort(name, args) => args.map(show).mkString(s"$name[", ", ", "]")

  // the Params and Metas in t
  def free(t: Type): List[Type] = t match
    case Type.Param(_) | Type.Meta(_) => List(t)
    case Type.Hole               => Nil
    case Type.Sort(_, args)      => args.flatMap(free).distinct

  def holes(t: Type): Boolean = t match
    case Type.Hole          => true
    case Type.Sort(_, args) => args.exists(holes)
    case _             => false

  def subst(t: Type, su: Map[Type, Type]): Type = t match
    case Type.Sort(name, args) => Type.Sort(name, args.map(subst(_, su)))
    case _                => su.getOrElse(t, t)

  def program(p: Program): Program = new Check(p).cmds()

  def expr(e: Expr): (Expr, Type) =
    val c = new Check(Program(Nil))
    val (e1, t) = c.infer(Map(), e)
    (c.zonk(e1), c.zonk(t))

class Check(program: Program):
  import Check.*

  val context = util.Ref(initial)
  val su = util.Ref(Map[Int, Type]())
  val next = util.Ref(0)

  def aliases = context.get.aliases
  def funs = context.get.funs
  def procs = context.get.procs
  def datas = context.get.datas
  def members = context.get.members
  def constrs = context.get.constrs

  def update(f: Ctx => Ctx): Unit =
    context.get = f(context.get)

  def error(msg: String): Nothing = throw new Error(s"type error: $msg")

  def require(cond: Boolean, msg: => String): Unit =
    if !cond then error(msg)

  def fresh(): Type =
    next.get += 1
    Type.Meta(next.get)

  def distinct(names: List[Name], what: String): Unit =
    val dups = names.diff(names.distinct)
    require(dups.isEmpty, s"duplicate $what ${dups.distinct.mkString(", ")}")

  def sameLength(xs: List[?], es: List[?], what: String): Unit =
    require(xs.length == es.length, s"$what expects ${xs.length} values, got ${es.length}")

  def arguments(name: Name, arity: Int, args: List[Type]): Unit =
    require(arity == args.length, s"sort $name expects $arity arguments, got ${args.length}")

  // expands sort definitions and replaces holes by fresh unknowns
  def normalize(t: Type): Type = t match
    case Type.Hole                              => fresh()
    case Type.Param(_) | Type.Meta(_)                => t
    case Type.Sort("->", args) if args.nonEmpty => Type.Sort("->", args.map(normalize))
    case Type.Sort(name, args) if basic(name) =>
      arguments(name, 0, args)
      t
    case Type.Sort(name, args) if datas.contains(name) =>
      arguments(name, datas(name), args)
      Type.Sort(name, args.map(normalize))
    case Type.Sort(name, args) =>
      val (params, rhs) = aliases.getOrElse(name, error(s"unknown sort $name"))
      arguments(name, params.length, args)
      subst(rhs, params.map(Type.Param(_)).zip(args.map(normalize)).toMap)

  def known(t: Type, what: String): Type =
    require(!holes(t), s"missing type for $what")
    normalize(t)

  def formals(xs: List[Pat.Var]): List[Pat.Var] =
    distinct(xs.names, "variable")
    xs.map(x => Pat.Var(x.name, normalize(x.typ)))

  def signature(xs: List[Pat.Var]): List[Pat.Var] =
    distinct(xs.names, "variable")
    xs.map(x => Pat.Var(x.name, known(x.typ, x.name)))

  def bind(xs: List[Pat.Var]): Map[Name, Scheme] = xs.map(x => x.name -> mono(x.typ)).toMap

  def resolve(t: Type): Type = t match
    case Type.Meta(index) if su.get.contains(index) => resolve(su.get(index))
    case _                                 => t

  def zonk(t: Type): Type = resolve(t) match
    case Type.Sort(name, args) => Type.Sort(name, args.map(zonk(_)))
    case t                => t

  def zonk(x: Expr.Var): Expr.Var = Expr.Var(x.name, zonk(x.typ))

  def zonk(x: Pat.Var): Pat.Var = Pat.Var(x.name, zonk(x.typ))

  def zonk(e: Expr): Expr = e match
    case Expr.Lit(_)                 => e
    case x: Expr.Var                 => zonk(x)
    case Expr.App(fun, args)         => Expr.App(zonk(fun), args.map(zonk(_)))
    case Expr.Lam(bound, body)       => Expr.Lam(bound.map(zonk(_)), zonk(body))
    case Expr.Let(bound, args, body) => Expr.Let(bound.map(zonk(_)), args.map(zonk(_)), zonk(body))
    case Expr.Ite(test, left, right) => Expr.Ite(zonk(test), zonk(left), zonk(right))
    case Expr.Match(arg, cases)      => Expr.Match(zonk(arg), cases.map(c => Case(zonk(c.pat), zonk(c.body))))

  def zonk(p: Pat): Pat = p match
    case Pat.Wild | Pat.Lit(_) => p
    case Pat.Var(name, typ)    => Pat.Var(name, zonk(typ))
    case Pat.App(name, args)   => Pat.App(name, args.map(zonk(_)))

  def zonk(s: Stmt): Stmt = s match
    case Stmt.Skip                        => s
    case Stmt.Assign(vars, args)          => Stmt.Assign(vars.map(zonk(_)), args.map(zonk(_)))
    case Stmt.Block(locals, stmts)        => Stmt.Block(locals.map(zonk(_)), stmts.map(zonk(_)))
    case Stmt.If(test, left, right)       => Stmt.If(zonk(test), zonk(left), zonk(right))
    case Stmt.While(test, body)           => Stmt.While(zonk(test), zonk(body))
    case Stmt.Call(proc, ins, outs, refs) => Stmt.Call(proc, ins.map(zonk(_)), outs.map(zonk(_)), refs.map(zonk(_)))
    case Stmt.Match(arg, cases)           => Stmt.Match(zonk(arg), cases.map(c => Case(zonk(c.pat), zonk(c.body))))

  def zonk(d: Cmd): Cmd = d match
    case Cmd.Sort(_, _, _) | Cmd.Data(_, _, _) => d
    case Cmd.Fun(name, bound, res, body)        => Cmd.Fun(name, bound.map(zonk(_)), zonk(res), zonk(body))
    case Cmd.Proc(name, ins, outs, refs, body)  => Cmd.Proc(name, ins.map(zonk(_)), outs.map(zonk(_)), refs.map(zonk(_)), zonk(body))
    case Cmd.Eval(expr)                         => Cmd.Eval(zonk(expr))
    case Cmd.Test(lhs, rhs)                     => Cmd.Test(zonk(lhs), zonk(rhs))

  def occurs(i: Int, t: Type): Boolean = resolve(t) match
    case Type.Meta(index)   => i == index
    case Type.Sort(_, args) => args.exists(occurs(i, _))
    case _             => false

  def unify(t1: Type, t2: Type): Unit =
    (resolve(t1), resolve(t2)) match
      case (Type.Meta(a), Type.Meta(b)) if a == b   => ()
      case (Type.Param(a), Type.Param(b)) if a == b => ()
      case (Type.Meta(index), t)               => assign(index, t)
      case (t, Type.Meta(index))               => assign(index, t)
      case (Type.Sort(f, as), Type.Sort(g, bs)) if f == g && as.length == bs.length =>
        unify(as, bs)
      case _ =>
        error(s"cannot unify ${show(zonk(t1))} and ${show(zonk(t2))}")

  def unify(as: List[Type], bs: List[Type]): Unit = (as, bs) match
    case (Nil, Nil)         =>
    case (a :: as, b :: bs) => unify(a, b); unify(as, bs)
    case _                  => error(s"cannot unify ${as.length} types with ${bs.length} types")

  def assign(i: Int, t: Type): Unit =
    require(!occurs(i, t), s"infinite type ?$i = ${show(zonk(t))}")
    su.get += i -> t

  def instance(params: List[Type]): Map[Type, Type] =
    params.map(_ -> fresh()).toMap

  def instantiate(s: Scheme): Type =
    subst(zonk(s.typ), instance(s.params))

  def lookup(env: Map[Name, Scheme], x: Name): Option[Scheme] =
    env.get(x).orElse(funs.get(x)).orElse(constrs.get(x).map(_.scheme)).orElse(builtins.get(x))

  // a use of x elaborated with an instance of s
  def use(x: Name, t: Type, s: Scheme): Expr.Var =
    val u = instantiate(s)
    unify(u, normalize(t))
    Expr.Var(x, u)

  def generalize(env: Map[Name, Scheme], t: Type): Scheme =
    val fixed = env.values.flatMap(s => free(zonk(s.typ)).diff(s.params)).toSet
    val typ = zonk(t)
    Scheme(free(typ).filter(p => p.isInstanceOf[Type.Meta] && !fixed(p)), typ)

  def infer(env: Map[Name, Scheme], e: Expr): (Expr, Type) = e match
    case Expr.Lit(())         => (e, Unit)
    case Expr.Lit(_: Boolean) => (e, Bool)
    case Expr.Lit(_: BigInt)  => (e, Int)
    case Expr.Lit(_: String)  => (e, Str)
    case Expr.Lit(value)      => error(s"invalid literal $value")

    case Expr.Var(name, typ) =>
      val v = use(name, typ, lookup(env, name).getOrElse(error(s"unknown variable $name")))
      (v, v.typ)

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

    case Expr.Match(arg, cases) =>
      val r = fresh()
      val (arg1, cases1) = branches(env, arg, cases)((bs, b) => check(env ++ bs, b, r))
      (Expr.Match(arg1, cases1), r)

  // checks the patterns against arg and each body with the variables its pattern binds
  def branches[A](env: Map[Name, Scheme], arg: Expr, cases: List[Case[A]])(elaborate: (Map[Name, Scheme], A) => A): (Expr, List[Case[A]]) =
    val (arg1, t) = infer(env, arg)
    val cases1 = for Case(pat, body) <- cases yield
      val (pat1, bs) = pattern(pat, t)
      Case(pat1, elaborate(bs, body))
    exhaustive(t, cases1)
    (arg1, cases1)

  def pattern(p: Pat, t: Type): (Pat, Map[Name, Scheme]) =
    val (p1, bs) = binders(p, t)
    distinct(bs.names, "pattern variable")
    (p1, bind(bs))

  def binders(p: Pat, t: Type): (Pat, List[Pat.Var]) = p match
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
      val k = constrs.getOrElse(name, error(s"unknown constructor $name"))
      sameLength(k.args, args, s"constructor $name")
      val su = instance(k.params)
      unify(subst(k.res, su), t)
      val (args1, bs) = binders(args, k.args.map(subst(_, su)))
      (Pat.App(name, args1), bs)

  def binders(ps: List[Pat], ts: List[Type]): (List[Pat], List[Pat.Var]) = (ps, ts) match
    case (Nil, Nil) => (Nil, Nil)
    case (p :: ps, t :: ts) =>
      val (p1, bs) = binders(p, t)
      val (ps1, cs) = binders(ps, ts)
      (p1 :: ps1, bs ++ cs)
    case _ => error(s"expected ${ts.length} patterns, got ${ps.length}")

  def exhaustive(t: Type, cases: List[Case[?]]): Unit =
    for w <- missing(cases.map(c => List(c.pat)), List(t)) do
      error(s"match is not exhaustive, missing ${w.head}")

  // the constructor of an elaborated pattern, literals count as constructors without arguments
  def head(p: Pat): Option[(Name, List[Pat])] = p match
    case Pat.App(name, args) => Some(name -> args)
    case Pat.Lit(value)      => Some(Run.show(value) -> Nil)
    case _                   => None

  def witness(c: Name, args: List[String]): String = (c, args) match
    case (",", _) => args.mkString("(", ", ", ")")
    case (_, Nil) => c
    case _        => args.mkString(s"$c(", ", ", ")")

  // all constructors of t with their argument types, if there are finitely many
  def signature(t: Type): Option[List[(Name, List[Type])]] = zonk(t) match
    case Type.Sort("Bool", Nil) => Some(List("true" -> Nil, "false" -> Nil))
    case Type.Sort("Unit", Nil) => Some(List("()" -> Nil))
    case Type.Sort(name, args) if datas.contains(name) =>
      Some(for c <- members(name) yield
        val k = constrs(c)
        c -> k.args.map(subst(_, k.params.zip(args).toMap)))
    case _ => None

  // values of types ts, shown as patterns, that no row matches
  def missing(rows: List[List[Pat]], ts: List[Type]): Option[List[String]] = ts match
    case Nil       => Option.when(rows.isEmpty)(Nil)
    case t :: rest => missing(rows, rows.flatMap(r => head(r.head)).map(_._1).toSet, signature(t), rest)

  // heads are the constructors in the first column, cs are all constructors of its type if there are finitely many
  def missing(rows: List[List[Pat]], heads: Set[Name], cs: Option[List[(Name, List[Type])]], rest: List[Type]): Option[List[String]] = cs match
    case Some(cs) if cs.forall((c, _) => heads(c)) =>
      cs.iterator
        .flatMap((c, args) => missing(specialize(rows, c, args.length), args ++ rest).map(ws => witness(c, ws.take(args.length)) :: ws.drop(args.length)))
        .nextOption()
    case _ =>
      missing(rows.filter(r => head(r.head).isEmpty).map(_.tail), rest).map(uncovered(heads, cs) :: _)

  // a value of the first column that heads do not cover
  def uncovered(heads: Set[Name], cs: Option[List[(Name, List[Type])]]): String = cs.flatMap(_.find((c, _) => !heads(c))) match
    case Some((c, args)) if heads.nonEmpty => witness(c, args.map(_ => "_"))
    case _                                 => "_"

  def specialize(rows: List[List[Pat]], c: Name, n: Int): List[List[Pat]] =
    for r <- rows if head(r.head).forall(_._1 == c) yield head(r.head) match
      case Some((_, ps)) => ps ++ r.tail
      case None          => List.fill(n)(Pat.Wild) ++ r.tail

  def check(env: Map[Name, Scheme], e: Expr, t: Type): Expr =
    val (e1, t1) = infer(env, e)
    unify(t1, t)
    e1

  def check(env: Map[Name, Scheme], es: List[Expr], ts: List[Type]): List[Expr] = (es, ts) match
    case (Nil, Nil)         => Nil
    case (e :: es, t :: ts) => check(env, e, t) :: check(env, es, ts)
    case _                  => error(s"expected ${ts.length} values, got ${es.length}")

  def infer(env: Map[Name, Scheme], es: List[Expr]): (List[Expr], List[Type]) =
    es.map(infer(env, _)).unzip

  def apply(env: Map[Name, Scheme], f: Type, args: List[Expr]): (List[Expr], Type) =
    val r = fresh()
    val (args1, ts) = infer(env, args)
    unify(f, fun(ts, r))
    (args1, r)

  def variable(env: Map[Name, Scheme], assignable: Set[Name], x: Expr.Var): Expr.Var =
    require(assignable(x.name), s"${x.name} is not assignable")
    val t = env(x.name).typ
    unify(t, normalize(x.typ))
    Expr.Var(x.name, t)

  def stmt(env: Map[Name, Scheme], assignable: Set[Name], s: Stmt): Stmt = s match
    case Stmt.Skip => s

    case Stmt.Assign(vars, args) =>
      distinct(vars.names, "assignment to")
      sameLength(vars, args, "assignment")
      val vars1 = vars.map(variable(env, assignable, _))
      Stmt.Assign(vars1, check(env, args, vars1.types))

    case Stmt.Block(locals, stmts) =>
      val xs = formals(locals)
      Stmt.Block(xs, stmts.map(stmt(env ++ bind(xs), assignable ++ xs.names, _)))

    case Stmt.If(test, left, right) =>
      Stmt.If(check(env, test, Bool), stmt(env, assignable, left), stmt(env, assignable, right))

    case Stmt.While(test, body) =>
      Stmt.While(check(env, test, Bool), stmt(env, assignable, body))

    case Stmt.Call(proc, ins, outs, refs) =>
      val sig = procs.getOrElse(proc, error(s"unknown procedure $proc"))
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

  def declare(name: Name): Unit =
    require(!funs.contains(name) && !procs.contains(name) && !constrs.contains(name) && !builtins.contains(name), s"$name is already defined")

  def declareSort(name: Name): Unit =
    require(!basic(name) && name != "->" && !aliases.contains(name) && !datas.contains(name), s"sort $name is already defined")

  def cmds(): Program =
    // datatypes may be mutually recursive, so they are known before any sort is checked
    for case Cmd.Data(name, params, _) <- program.cmds do
      declareSort(name)
      update(c => c.copy(datas = c.datas + (name -> params.length)))

    def closed(name: Name, params: List[Name], t: Type): Unit =
      val undeclared = free(t).diff(params.map(Type.Param(_)))
      require(undeclared.isEmpty, s"sort $name has undeclared type parameters ${undeclared.map(show).mkString(", ")}")

    for case Cmd.Sort(name, params, typ) <- program.cmds do
      declareSort(name)
      distinct(params, "type parameter")
      val rhs = known(typ, s"sort $name")
      closed(name, params, rhs)
      update(c => c.copy(aliases = c.aliases + (name -> (params, rhs))))

    for case Cmd.Data(name, params, cs) <- program.cmds do
      distinct(params, "type parameter")
      val ps = params.map(Type.Param(_))
      update(c => c.copy(members = c.members + (name -> cs.map(_.name))))
      for Constr(c, args) <- cs do
        declare(c)
        val ts = args.map(known(_, s"constructor $c"))
        for t <- ts do closed(name, params, t)
        update(ctx => ctx.copy(constrs = ctx.constrs + (c -> Constructor(ps, ts, Type.Sort(name, ps)))))

    for case Cmd.Fun(name, bound, res, _) <- program.cmds do
      declare(name)
      val t = fun(signature(bound).types, known(res, s"result of $name"))
      update(c => c.copy(funs = c.funs + (name -> Scheme(free(t), t))))

    for case Cmd.Proc(name, ins, outs, refs, _) <- program.cmds do
      declare(name)
      val ts = signature(ins ++ outs ++ refs).types
      val (is, rest) = ts.splitAt(ins.length)
      val (os, rs) = rest.splitAt(outs.length)
      update(c => c.copy(procs = c.procs + (name -> Sig(ts.flatMap(free).distinct, is, os, rs))))

    val cmds = program.cmds.map:
      case d: Cmd.Sort => d
      case d: Cmd.Data => d

      case Cmd.Fun(name, bound, res, body) =>
        val xs = signature(bound)
        val t = known(res, s"result of $name")
        Cmd.Fun(name, xs, t, check(bind(xs), body, t))

      case Cmd.Proc(name, ins, outs, refs, body) =>
        val is = signature(ins)
        val os = signature(outs)
        val rs = signature(refs)
        Cmd.Proc(name, is, os, rs, stmt(bind(is ++ os ++ rs), (os ++ rs).names.toSet, body))

      case Cmd.Eval(expr) =>
        Cmd.Eval(infer(Map(), expr)._1)

      case Cmd.Test(lhs, rhs) =>
        val (lhs1, t) = infer(Map(), lhs)
        Cmd.Test(lhs1, check(Map(), rhs, t))

    Program(cmds.map(zonk(_)))
