package ulang

case class Obj(name: Name, args: List[Any])

class Cell(get: Option[Any]) extends Ref[Option[Any]](get)

// an exception of type typ raised at pos, which try catches
class Raised(val value: Any, val typ: Type, val pos: Pos) extends Exception with scala.util.control.NoStackTrace

object Run:
  def int(v: Any): BigInt =
    v match
      case n: BigInt =>
        n
      case _ =>
        error("runtime", s"expected an integer, got ${Printer.show(v)}")

  def bool(v: Any): Boolean =
    v match
      case b: Boolean =>
        b
      case _ =>
        error("runtime", s"expected a boolean, got ${Printer.show(v)}")

  def equal(a: Any, b: Any): Boolean =
    (a, b) match
      case (_: Function1[?, ?], _) | (_, _: Function1[?, ?]) =>
        error("runtime", "cannot compare functions")
      case (Obj(c, as), Obj(d, bs)) =>
        c == d && equal(as, bs)
      case _ =>
        a == b

  def equal(as: List[Any], bs: List[Any]): Boolean =
    (as, bs) match
      case (Nil, Nil) =>
        true
      case (a :: as, b :: bs) =>
        equal(a, b) && equal(as, bs)
      case _ =>
        false

  def string(v: Any): String =
    v match
      case s: String =>
        s
      case _ =>
        error("runtime", s"expected a string, got ${Printer.show(v)}")

  def list(vs: List[Any]): Any =
    vs.foldRight(Obj(Names.nil, Nil): Any)((v, rest) => Obj(Names.cons, List(v, rest)))

  def character(v: Any): Char =
    v match
      case c: Char =>
        c
      case _ =>
        error("runtime", s"expected a character, got ${Printer.show(v)}")

  def elements(v: Any): List[Any] =
    Printer.items(v).getOrElse(error("runtime", s"expected a list, got ${Printer.show(v)}"))

  def function(v: Any): List[Any] => Any =
    v match
      case f: Function1[?, ?] =>
        f.asInstanceOf[List[Any] => Any]
      case _ =>
        error("runtime", s"cannot apply ${Printer.show(v)}")

  def bind[A](xs: List[Pat.Var], vs: List[A]): Map[Name, A] =
    xs.names.zip(vs).toMap

  def cell(v: Any): Cell =
    Cell(Some(v))

  // an exception of a builtin, of a type of the prelude
  def raise(name: Name, args: List[Any], pos: Pos): Nothing =
    throw Raised(Obj(name, args), Type.Sort(name, Nil), pos)

  def nonzero(v: Any, pos: Pos): BigInt =
    if int(v) == 0 then raise(Names.divisionByZero, Nil, pos)
    int(v)

  // the index i of s, which must be within s, or at its end if end is true
  def index(s: String, i: Any, end: Boolean, pos: Pos): Int =
    if int(i) < 0 || int(i) > s.length || int(i) == s.length && !end then raise(Names.noIndex, List(s, int(i)), pos)
    int(i).toInt

  // where builtins that fail raise exceptions at pos, the position of the call
  def builtin(f: Id, pos: Pos)(args: List[Any]): Any =
    (f, args) match
      case ("+", List(a, b)) =>
        int(a) + int(b)
      case ("-", List(a, b)) =>
        int(a) - int(b)
      case ("*", List(a, b)) =>
        int(a) * int(b)
      case ("/", List(a, b)) =>
        int(a) / nonzero(b, pos)
      case ("%", List(a, b)) =>
        int(a) % nonzero(b, pos)
      case ("<", List(a, b)) =>
        int(a) < int(b)
      case ("==", List(a, b)) =>
        equal(a, b)
      case ("chars", List(s: String)) =>
        list(s.toList)
      case ("string", List(cs)) =>
        elements(cs).map(character).mkString
      case ("ord", List(c: Char)) =>
        BigInt(c.toInt)
      case ("chr", List(n)) =>
        if int(n) < 0 || int(n) > Char.MaxValue.toInt then raise(Names.noCharacter, List(int(n)), pos)
        int(n).toChar
      case ("^", List(s: String, t: String)) =>
        s + t
      case ("str_size", List(s: String)) =>
        BigInt(s.length)
      case ("str_join", List(sep: String, ss)) =>
        elements(ss).map(string).mkString(sep)
      // from i up to but excluding j, where i <= j are indices of s or its end
      case ("str_slice", List(s: String, i, j)) =>
        val (a, b) = (index(s, i, true, pos), index(s, j, true, pos))
        if a > b then raise(Names.noIndex, List(s, BigInt(a)), pos)
        s.substring(a, b)
      case ("str_split", List(s: String, c: Char)) =>
        list(s.split(java.util.regex.Pattern.quote(c.toString), -1).toList)
      case ("str_get", List(s: String, i)) =>
        s(index(s, i, false, pos))
      case ("str_contains", List(s: String, c: Char)) =>
        s.contains(c)
      // -1, 0, or 1 as s is before, equal to, or after t, by the codes of their characters
      case ("str_compare", List(s: String, t: String)) =>
        BigInt(Integer.signum(s.compareTo(t)))
      case ("str_show", List(n)) =>
        int(n).toString
      case ("str_quote", List(s: String)) =>
        Printer.show(s)
      case ("str_number", List(s: String)) =>
        if "-?[0-9]+".r.matches(s) then Obj(Names.some, List(BigInt(s))) else Obj(Names.none, Nil)
      case ("is_digit", List(c: Char)) =>
        '0' <= c && c <= '9'
      case ("is_letter", List(c: Char)) =>
        'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'
      case ("is_space", List(c: Char)) =>
        c == ' ' || c == '\n' || c == '\t' || c == '\r'
      case _ =>
        error("runtime", s"$f${args.map(Printer.show).mkString("(", ", ", ")")} is unspecified")

// dir is the directory of the program, which the paths of read are relative to
class Run(val program: Program, val dir: os.Path = os.pwd, val args: List[String] = Nil):
  import Run.*

  // functions are overloaded by their number of arguments
  val funs =
    program.cmds.collect:
      case d: Cmd.Fun =>
        (d.name, d.bound.head.length) -> d
    .toMap

  val procs =
    program.cmds.collect:
      case d: Cmd.Proc =>
        d.name -> d
    .toMap

  val constrs =
    program.cmds.collect:
      case d: Cmd.Data =>
        d.constrs.map(c => c.name -> c.args.length)
    .flatten.toMap

  // the position of each constructor in its declaration, which orders values
  val positions =
    program.cmds.collect:
      case d: Cmd.Data =>
        d.constrs.map(_.name).zipWithIndex
    .flatten.toMap

  // values of the same type, where constructors are ordered by their declaration and then by their fields
  def compare(a: Any, b: Any): Int =
    (a, b) match
      case (_: Function1[?, ?], _) | (_, _: Function1[?, ?]) =>
        error("runtime", "cannot compare functions")
      case (x: BigInt, y: BigInt) =>
        x.compare(y).sign
      case (x: Boolean, y: Boolean) =>
        x.compare(y).sign
      case (x: Char, y: Char) =>
        x.compare(y).sign
      case (x: String, y: String) =>
        x.compareTo(y).sign
      case (Obj(c, as), Obj(d, bs)) if c == d =>
        compare(as, bs)
      case (Obj(c, _), Obj(d, _)) =>
        positions.getOrElse(c, 0).compare(positions.getOrElse(d, 0)).sign
      case _ =>
        error("runtime", s"cannot compare ${Printer.show(a)} and ${Printer.show(b)}")

  def compare(as: List[Any], bs: List[Any]): Int =
    (as, bs) match
      case (a :: as, b :: bs) =>
        compare(a, b) match
          case 0 =>
            compare(as, bs)
          case c =>
            c
      case _ =>
        as.length.compare(bs.length).sign

  def eval(env: Map[Name, Any], e: Expr): Any =
    at(e.pos):
      e match
        case Expr.Lit(value) =>
          value

        case Expr.Var(name, _) if env.contains(name) =>
          env(name)
        case Expr.Var(name, typ) if funs.contains((name, Check.arity(typ))) =>
          fun(funs((name, Check.arity(typ))))
        case Expr.Var(name, _) if constrs.contains(name) =>
          constr(name)(constrs(name))
        // compare needs the order of the constructors of the program
        case Expr.Var(Name(None, "compare"), _) =>
          (vs: List[Any]) =>
            vs match
              case List(a, b) =>
                BigInt(compare(a, b))
              case _ =>
                error("runtime", "compare takes two arguments")
        case Expr.Var(name, _) if name.module.isEmpty && Check.builtins.contains(name.base) =>
          builtin(name.base, e.pos)
        case Expr.Var(name, _) =>
          error("runtime", s"$name is not initialized")

        // and, or are not strict and therefore not builtins
        case Expr.App(Expr.Var(Name(None, "and"), _), List(a, b)) =>
          bool(eval(env, a)) && bool(eval(env, b))
        case Expr.App(Expr.Var(Name(None, "or"), _), List(a, b)) =>
          bool(eval(env, a)) || bool(eval(env, b))

        case Expr.App(fun, args) =>
          function(eval(env, fun))(args.map(eval(env, _)))

        case Expr.Lam(bound, body) =>
          (vs: List[Any]) => eval(env ++ bind(bound, vs), body)

        // the variables are bound in parallel
        case Expr.Let(bound, body) =>
          eval(env ++ bound.map((x, a) => x.name -> eval(env, a)), body)

        case Expr.Ite(test, left, right) =>
          if bool(eval(env, test)) then
            eval(env, left)
          else
            eval(env, right)

        case Expr.Match(arg, cases) =>
          val (b, bs) = select(env, eval(env, arg), cases)
          eval(env ++ bs, b)

        case Expr.Comp(result, bound, lists, test) =>
          val vs =
            for
              env1 <- combinations(env, bound, lists)
              if bool(eval(env1, test))
            yield eval(env1, result)
          list(vs)

        case Expr.Tuple(items) =>
          Obj(Names.tuple, items.map(eval(env, _)))

        case Expr.Raise(arg, typ) =>
          throw Raised(eval(env, arg), typ, e.pos)

  def constr(c: Name)(arity: Int): Any =
    if arity == 0 then
      Obj(c, Nil)
    else
      (vs: List[Any]) => Obj(c, vs)

  // env extended by all combinations of elements of lists
  def combinations(env: Map[Name, Any], xs: List[Pat.Var], lists: List[Expr]): List[Map[Name, Any]] =
    (xs, lists) match
      case (Nil, Nil) =>
        List(env)
      case (x :: xs, list :: lists) =>
        for
          v <- elements(eval(env, list))
          env1 <- combinations(env + (x.name -> v), xs, lists)
        yield env1
      case _ =>
        error("runtime", s"expected ${xs.length} lists, got ${lists.length}")

  // the first case whose pattern matches v and whose guard holds
  def select[A](env: Map[Name, Any], v: Any, cases: List[Case[A]]): (A, Map[Name, Any]) =
    cases match
      case Nil =>
        error("runtime", s"no case matches ${Printer.show(v)}")
      case c :: cs =>
        matches(c.pat, v) match
          case Some(bs) if holds(env ++ bs, c.guard) =>
            (c.body, bs)
          case _ =>
            select(env, v, cs)

  def holds(env: Map[Name, Any], guard: Option[Expr]): Boolean =
    guard match
      case Some(g) =>
        bool(eval(env, g))
      case None =>
        true

  def matches(p: Pat, v: Any): Option[Map[Name, Any]] =
    (p, v) match
      case (Pat.Wild(), _) =>
        Some(Map())
      case (Pat.Lit(value), _) =>
        Option.when(equal(value, v))(Map())
      case (Pat.Var(name, _), _) if constrs.contains(name) =>
        matches(Pat.App(name, Nil)(p.pos), v)
      case (Pat.Var(name, _), _) =>
        Some(Map(name -> v))
      case (Pat.App(c, ps), Obj(d, vs)) if c == d =>
        matches(ps, vs)
      case (Pat.App(_, _), _) =>
        None
      case (Pat.Tuple(ps), Obj(Names.tuple, vs)) =>
        matches(ps, vs)
      case (Pat.Tuple(_), _) =>
        None

  def matches(ps: List[Pat], vs: List[Any]): Option[Map[Name, Any]] =
    (ps, vs) match
      case (Nil, Nil) =>
        Some(Map())
      case (p :: ps, v :: vs) =>
        for
          bs <- matches(p, v);
          cs <- matches(ps, vs)
        yield bs ++ cs
      case _ =>
        None

  def fun(d: Cmd.Fun): List[Any] => Any =
    d.bound match
      case List(Nil) =>
        _ => cached(d)
      case _ =>
        curried(Map(), d.bound, d.body)

  // the values of functions without parameters, which are computed once, since expressions are deterministic,
  // where None marks a value that is being computed, which detects a value that needs itself,
  // and an exception that leaves the computation removes the mark, such that the next call computes it again
  val cache = scala.collection.mutable.Map[Name, Option[Any]]()

  def cached(d: Cmd.Fun): Any =
    cache.get(d.name) match
      case Some(Some(v)) =>
        v
      case Some(None) =>
        error("runtime", s"recursive initialization of ${d.name}")
      case None =>
        cache(d.name) = None
        try
          val v = eval(Map(), d.body)
          cache(d.name) = Some(v)
          v
        catch
          case e: Throwable =>
            cache.remove(d.name)
            throw e

  // a function for the first of bound, which returns functions for the others, and finally evaluates body
  def curried(env: Map[Name, Any], bound: List[List[Pat.Var]], body: Expr): List[Any] => Any =
    bound match
      case xs :: Nil =>
        vs => eval(env ++ bind(xs, vs), body)
      case xs :: rest =>
        vs => curried(env ++ bind(xs, vs), rest, body)
      case Nil =>
        error("runtime", "a function without parameters")

  def values(store: Map[Name, Cell]): Map[Name, Any] =
    store.flatMap((x, r) => r.get.map(x -> _))

  def value(store: Map[Name, Cell], e: Expr): Any =
    eval(values(store), e)

  def exec(store: Map[Name, Cell], s: Stmt): Unit =
    at(s.pos):
      s match
        case Stmt.Skip() =>

        // the variables are assigned in parallel
        case Stmt.Assign(pairs) =>
          val env = values(store)
          val (vars, args) = pairs.unzip
          assign(store, vars, args.map(eval(env, _)))

        case Stmt.Block(locals, stmts) =>
          val inner = store ++ locals.map(_.name -> Cell(None))
          for s <- stmts do
            exec(inner, s)

        case Stmt.If(test, left, right) =>
          if bool(value(store, test)) then
            exec(store, left)
          else
            exec(store, right)

        case Stmt.While(test, body) =>
          while bool(value(store, test)) do
            exec(store, body)

        case Stmt.Call(proc, ins, outs, refs) =>
          val env = values(store)
          assign(store, outs, invoke(proc, ins.map(eval(env, _)), refs.map(x => store(x.name)), s.pos))

        case Stmt.Match(arg, cases) =>
          val (b, bs) = select(values(store), value(store, arg), cases)
          exec(store ++ bs.view.mapValues(cell), b)

        // assertions of equations show both sides when they fail
        case Stmt.Assert(Expr.App(Expr.Var(Name(None, "=="), _), List(lhs, rhs))) =>
          val (a, b) = (value(store, lhs), value(store, rhs))
          require("runtime", equal(a, b), s"assertion failed: ${Printer.show(a)} != ${Printer.show(b)}")

        case Stmt.Assert(test) =>
          require("runtime", bool(value(store, test)), "assertion failed")

        case Stmt.Raise(arg, typ) =>
          throw Raised(value(store, arg), typ, s.pos)

        case Stmt.Try(body, handlers) =>
          try
            exec(store, body)
          catch
            case e: Raised if handler(values(store), e, handlers).nonEmpty =>
              val (b, bs) = handler(values(store), e, handlers).get
              exec(store ++ bs.view.mapValues(cell), b)

  // the first handler of e whose guard holds, where _ handles exceptions of any type
  def handler(env: Map[Name, Any], e: Raised, handlers: List[Handler]): Option[(Stmt, Map[Name, Any])] =
    handlers match
      case Nil =>
        None
      case Handler(_, _: Pat.Wild, guard, body) :: rest =>
        if holds(env, guard) then
          Some(body -> Map())
        else
          handler(env, e, rest)
      case Handler(typ, pat, guard, body) :: rest if typ == e.typ =>
        matches(pat, e.value) match
          case Some(bs) if holds(env ++ bs, guard) =>
            Some(body -> bs)
          case _ =>
            handler(env, e, rest)
      case _ :: rest =>
        handler(env, e, rest)

  def assign(store: Map[Name, Cell], xs: List[Expr.Var], vs: List[Any]): Unit =
    (xs, vs) match
      case (Nil, Nil)         =>
      case (x :: xs, v :: vs) =>
        store(x.name).get = Some(v)
        assign(store, xs, vs)
      case _ =>
        error("runtime", s"cannot assign ${vs.length} values to ${xs.length} variables")

  // the builtin procedures have no module
  def invoke(p: Name, ins: List[Any], refs: List[Cell], pos: Pos): List[Any] =
    (procs.get(p), p.base, ins) match
      case (Some(d), _, _) =>
        val outs = d.outs.map(_ => Cell(None))
        exec(bind(d.ins, ins.map(cell)) ++ bind(d.outs, outs) ++ bind(d.refs, refs), d.body)
        for (x, r) <- d.outs.zip(outs)
        yield r.get.getOrElse(error("runtime", s"output ${x.name} of $p is not assigned"))
      case (None, "read", List(path: String)) =>
        List(read(path, pos))
      case (None, "library", List(name: String)) =>
        List(library(name))
      case (None, "os_args", Nil) =>
        List(list(args))
      case (None, "os_read", List(path: String)) =>
        try List(os.read(os.Path(path, dir)))
        catch case _: java.io.IOException => throw osError(s"cannot read $path", pos)
      case (None, "os_write", List(path: String, text: String)) =>
        try os.write.over(os.Path(path, dir), text, createFolders = true)
        catch case _: java.io.IOException => throw osError(s"cannot write $path", pos)
        Nil
      case (None, "os_library", List(name: String)) =>
        List(Main.libraryText(name).getOrElse(throw osError(s"unknown module $name", pos)))
      case _ =>
        error("runtime", s"unknown procedure $p")

  def osError(message: String, pos: Pos): Raised =
    Raised(Obj(Names.osError, List(message)), Type.Sort(Names.osError, Nil), pos)

  // the module that declares the syntax of self/syntax.u, whose values read and library give
  def quote: Quote =
    val module =
      program.cmds.collectFirst:
        case Cmd.Data(Name(Some(m), "Program"), _, _) =>
          m
    Quote(module.getOrElse(error("runtime", "the program does not declare Program")))

  // the syntax of a file as a value of self/syntax.u, unchecked
  def read(path: String, pos: Pos): Any =
    val file = os.Path(path, dir)
    if !os.isFile(file) then throw osError(s"cannot read $path", pos)
    quote.quote(Main.parse(os.read(file), file))

  // the syntax of the prelude, or of another module of the library parsed after the prelude, unchecked
  def library(name: String): Any =
    if name == "prelude" then
      quote.quote(Main.prelude)
    else
      quote.quote(Parser.parseProgram(Main.library(name), Main.scope(name, Main.library)))

  // the values of evals and of the variables of runs, failed assertions are errors
  def run(): List[String] =
    program.cmds.flatMap(output)

  def output(c: Cmd): List[String] =
    try
      c match
        case Cmd.Eval(expr) =>
          List(Printer.show(eval(Map(), expr)))
        case Cmd.Run(_, body) =>
          results(body)
        case Cmd.Test(_, body) =>
          exec(Map(), body)
          Nil
        case _ =>
          Nil
    catch
      case e: Raised =>
        error("runtime", s"uncaught exception ${Printer.show(e.value)}", e.pos)

  // executes a block and returns the assigned values of its variables
  def results(body: Stmt): List[String] =
    body match
      case Stmt.Block(locals, stmts) =>
        val store = bind(locals, locals.map(_ => Cell(None)))
        for s <- stmts do
          exec(store, s)
        for
          x <- locals;
          v <- store(x.name).get.toList
        yield s"${x.name} = ${Printer.show(v)}"
      case _ =>
        exec(Map(), body)
        Nil
