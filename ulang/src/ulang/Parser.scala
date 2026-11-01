package ulang

// params are the type parameters in scope, which distinguish Param from Sort,
// notations are the operators declared so far, loaded are the modules imported so far,
// and read gives the text of a module
case class Scope(params: Set[Name], notations: List[Cmd.Notation], loaded: Set[Name] = Set(), read: Name => String = Scope.none):
  def prefix(op: Name): Option[Int] =
    notations.collectFirst:
      case Cmd.Notation(Fixity.Prefix, prec, names) if names.contains(op) =>
        prec

  def postfix(op: Name): Option[Int] =
    notations.collectFirst:
      case Cmd.Notation(Fixity.Postfix, prec, names) if names.contains(op) =>
        prec

  def infix(op: Name): Option[(Fixity, Int)] =
    notations.collectFirst:
      case Cmd.Notation(fixity @ (Fixity.Infix | Fixity.Left | Fixity.Right), prec, names) if names.contains(op) =>
        (fixity, prec)

  def declared(op: Name): Boolean =
    notations.exists(_.names.contains(op))

object Scope:
  def none(name: Name): String =
    error("parse", s"unknown module $name")

// operands and operators of an expression or type, before their precedence is resolved,
// where operators remember their input for errors
enum Item[A]:
  case Operand(a: A)
  case Operator(name: Name, at: scala.util.parsing.input.Reader[Char])

// operators that cannot be resolved, with the input of the offending operator if there is one
case class Unresolved(msg: String, at: Option[scala.util.parsing.input.Reader[Char]]) extends Exception(msg)

object Parser extends scala.util.parsing.combinator.RegexParsers:
  override val whiteSpace = """(\s|//.*)+""".r

  val keywords = Set(
    "_",
    "assert",
    "assign",
    "call",
    "case",
    "catch",
    "data",
    "else",
    "end",
    "eval",
    "false",
    "for",
    "function",
    "if",
    "import",
    "in",
    "infix",
    "lambda",
    "let",
    "local",
    "match",
    "out",
    "postfix",
    "prefix",
    "procedure",
    "raise",
    "ref",
    "run",
    "skip",
    "sort",
    "test",
    "then",
    "true",
    "try",
    "while"
  )

  // symbols with a fixed meaning, which cannot be operators
  val reserved = Set("=", "=>", ":=", ":", "|")

  // the input at the next token
  def skip(in: Input): Input =
    in.drop(handleWhiteSpace(in.source, in.offset) - in.offset)

  // the position of the next token
  val at: Parser[Pos] = Parser(in => Success(skip(in).pos, in))

  // p, given the position of its first token
  def located[A](p: Pos => Parser[A]): Parser[A] =
    at >> p

  def kw(word: String): Parser[String] =
    s"$word\\b".r

  val name: Parser[Name] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^? (
      {
        case x if !keywords(x) =>
          x
      },
      x => s"keyword $x is not a name"
    )

  val symbol: Parser[Name] =
    "[-+*/%<>=!&|:^~?@#$]+".r ^? (
      {
        case x if !reserved(x) =>
          x
      },
      x => s"$x is not an operator"
    )

  // a name that is not declared as an operator
  def plain(s: Scope): Parser[Name] =
    name ^? (
      {
        case x if !s.declared(x) =>
          x
      },
      x => s"operator $x is not a name"
    )

  // a symbol, or a name declared as an operator
  def operator(s: Scope): Parser[Name] =
    symbol |
      name ^? {
        case x if s.declared(x) =>
          x
      }

  val names: Parser[List[Name]] = rep1sep(name, ",")

  def list[A](p: => Parser[A]): Parser[List[A]] =
    repsep(p, ",")

  def parens[A](p: => Parser[A]): Parser[A] =
    "(" ~> p <~ ")"

  // (a) is just a, while () and (a, b, c) are tuples
  def tuple[A](p: => Parser[A])(tuple: List[A] => A): Parser[A] =
    parens(list(p)) ^^ {
      case List(a) =>
        a
      case as =>
        tuple(as)
    }

  // in ins, out outs, ref refs, where each section starts with its keyword, and empty sections are omitted
  def sections[A, B](in: Parser[A], rest: Parser[B]): Parser[(List[A], List[B], List[B])] =
    val ins = kw("in") ~> rep1sep(in, ",")
    val outs = kw("out") ~> rep1sep(rest, ",")
    val refs = kw("ref") ~> rep1sep(rest, ",")
    def tail[C](p: Parser[List[C]]): Parser[List[C]] =
      opt("," ~> p) ^^ (_.getOrElse(Nil))
    ins ~ tail(outs) ~ tail(refs) ^^ { case is ~ os ~ rs =>
      (is, os, rs)
    } | outs ~ tail(refs) ^^ { case os ~ rs =>
      (Nil, os, rs)
    } | refs ^^ (rs => (Nil, Nil, rs)) | success((Nil, Nil, Nil))

  // a sequence of operands and operators
  def items[A](s: Scope, operand: => Parser[A]): Parser[List[Item[A]]] =
    rep1(operand ^^ (Item.Operand(_)) | Parser(in => operator(s)(in).map(Item.Operator[A](_, skip(in)))))

  // resolves the precedence of operators by precedence climbing, or else stops parsing with a message,
  // since no other parse of the operators and operands can succeed
  def resolved[A](
      s: Scope,
      items: Parser[List[Item[A]]]
  )(prefix: (Name, A) => A, postfix: (Name, A) => A, infix: (Name, A, A) => A): Parser[A] =
    Parser: in =>
      items(in) match
        case Success(is, rest) =>
          try
            Success(Resolve(s, prefix, postfix, infix).all(is), rest)
          catch
            case Unresolved(msg, at) =>
              Error(msg, at.getOrElse(skip(rest)))
        case failure: NoSuccess =>
          failure

  // the components of a tuple type A * B * C while its operators are resolved, such that (A * B) * C stays nested
  val product = "(product)"

  // a single type of a list, where () is the empty tuple, and tuples are not written (A, B)
  def one(ts: List[Type]): Type =
    ts match
      case List(Type.Sort(`product`, cs)) =>
        Type.Tuple(cs)
      case List(t) =>
        t
      case Nil =>
        Type.Tuple(Nil)
      case _ =>
        throw Unresolved("tuple types are written A * B", None)

  def components(ts: List[Type]): List[Type] =
    ts match
      case List(Type.Sort(`product`, cs)) =>
        cs
      case _ =>
        List(one(ts))

  def typ(s: Scope): Parser[Type] =
    resolved(s, items(s, basic(s)))(
      (op, t) => List(Type.Sort(op, List(one(t)))),
      (op, t) => List(Type.Sort(op, List(one(t)))),
      (op, l, r) =>
        if op == "->" then
          List(Type.Fun(one(l), one(r)))
        else if op == "*" then
          List(Type.Sort(product, components(l) ++ components(r)))
        else
          List(Type.Sort(op, List(one(l), one(r))))
    ) ^? (
      {
        case ts if ts.length <= 1 =>
          one(ts)
      },
      _ => "tuple types are written A * B"
    )

  // a type, a type in parentheses, or () for the empty tuple
  def basic(s: Scope): Parser[List[Type]] =
    plain(s) ~ opt("[" ~> rep1sep(typ(s), ",") <~ "]") ^^ {
      case n ~ None if s.params(n) =>
        List(Type.Param(n))
      case n ~ args =>
        List(Type.Sort(n, args.getOrElse(Nil)))
    } | parens(list(typ(s)))

  val typarams: Parser[List[Name]] = opt("[" ~> names <~ "]") ^^ (_.getOrElse(Nil))

  def variable(s: Scope): Parser[Expr.Var] =
    located(pos => plain(s) ^^ (x => Expr.Var(x, Type.Hole)(pos)))

  // only parameters and locals have type annotations
  def untyped(s: Scope): Parser[Pat.Var] =
    located(pos => plain(s) ^^ (x => Pat.Var(x, Type.Hole)(pos)))

  def formal(s: Scope): Parser[Pat.Var] =
    located(pos =>
      plain(s) ~ opt(":" ~> typ(s)) ^^ { case x ~ t =>
        Pat.Var(x, t.getOrElse(Type.Hole))(pos)
      }
    )

  def formals(s: Scope): Parser[List[Pat.Var]] =
    list(formal(s))

  def expr(s: Scope): Parser[Expr] =
    operators(s)

  def exprs(s: Scope): Parser[List[Expr]] =
    rep1sep(expr(s), ",")

  def operators(s: Scope): Parser[Expr] =
    resolved(s, items(s, app(s)))(op(_, _), op(_, _), op(_, _, _))

  // let x := e, y := f in b binds variables in parallel, other patterns are matched against the tuple of values
  def let(s: Scope): Parser[Expr] =
    located: pos =>
      (kw("let") ~> rep1sep(pattern(s) ~ (":=" ~> expr(s)), ",")) ~
        (kw("in") ~> expr(s)) ^^ { case bs ~ b =>
          val (xs, es) = bs.map { case x ~ e =>
            (x, e)
          }.unzip
          xs match
            case _ if xs.forall(_.isInstanceOf[Pat.Var]) =>
              Expr.Let(
                xs.collect { case x: Pat.Var =>
                  x
                }.zip(es),
                b
              )(pos)
            case _ =>
              (xs, es) match
                case (List(x), List(e)) =>
                  Expr.Match(e, List(Case(x, b)))(pos)
                case _ =>
                  Expr.Match(Expr.Tuple(es)(pos), List(Case(Pat.Tuple(xs)(pos), b)))(pos)
        }

  def ite(s: Scope): Parser[Expr] =
    located: pos =>
      (kw("if") ~> expr(s)) ~
        (kw("then") ~> expr(s)) ~
        (kw("else") ~> expr(s)) ^^ { case c ~ l ~ r =>
          Expr.Ite(c, l, r)(pos)
        }

  def lam(s: Scope): Parser[Expr] =
    located: pos =>
      (kw("lambda") ~> repsep(untyped(s), ",")) ~
        ("=>" ~> expr(s)) ^^ { case xs ~ b =>
          Expr.Lam(xs, b)(pos)
        }

  def matches[A, B](s: Scope, body: => Parser[A])(make: (Expr, List[Case[A]], Pos) => B): Parser[B] =
    located: pos =>
      (kw("match") ~> expr(s)) ~
        (rep(kw("case") ~> pattern(s) ~ ("=>" ~> body)) <~ kw("end")) ^^ { case e ~ cs =>
          make(
            e,
            cs.map { case p ~ b =>
              Case(p, b)
            },
            pos
          )
        }

  // operators in patterns are constructors
  def pattern(s: Scope): Parser[Pat] =
    resolved(s, items(s, simple(s)))(
      (op, p) => Pat.App(op, List(p))(p.pos),
      (op, p) => Pat.App(op, List(p))(p.pos),
      (op, l, r) => Pat.App(op, List(l, r))(l.pos)
    )

  def simple(s: Scope): Parser[Pat] =
    located: pos =>
      plain(s) ~ parens(list(pattern(s))) ^^ { case c ~ ps =>
        Pat.App(c, ps)(pos)
      } |
        value ^^
        (Pat.Lit(_)(pos)) | tuple(pattern(s))(Pat.Tuple(_)(pos)) | kw("_") ^^^ Pat.Wild()(pos) |
        plain(s) ^^
        (Pat.Var(_, Type.Hole)(pos))

  // applications of operators are at their first operand
  def op(f: Name, args: Expr*): Expr =
    Expr.App(Expr.Var(f, Type.Hole)(args.head.pos), args.toList)(args.head.pos)

  def app(s: Scope): Parser[Expr] =
    atom(s) ~ rep(parens(list(expr(s)))) ^^ { case f ~ argss =>
      argss.foldLeft(f)(Expr.App(_, _)(f.pos))
    }

  val value: Parser[Any] =
    kw("true") ^^^ true | kw("false") ^^^ false |
      """\d+""".r ^^
      (BigInt(_)) |
      "\"[^\"]*\"".r ^^
      (s => s.substring(1, s.length - 1))

  // let, if, and lambda extend as far to the right as possible, also as the last operand of an operator
  def atom(s: Scope): Parser[Expr] =
    located: pos =>
      let(s) | ite(s) | lam(s) | matches(s, expr(s))(Expr.Match(_, _)(_)) |
        value ^^
        (Expr.Lit(_)(pos)) | tuple(expr(s))(Expr.Tuple(_)(pos)) | comp(s) |
        "[" ~> list(expr(s)) <~ "]" ^^
        (_.foldRight(Expr.Var("Nil", Type.Hole)(pos): Expr)(op("::", _, _))) | variable(s)

  // [result for x in xs, y in ys if test], where if test is optional
  def comp(s: Scope): Parser[Expr] =
    located: pos =>
      ("[" ~> expr(s)) ~
        (kw("for") ~> rep1sep(untyped(s) ~ (kw("in") ~> expr(s)), ",")) ~
        (opt(kw("if") ~> expr(s)) <~ "]") ^^ { case e ~ gs ~ t =>
          Expr.Comp(e, gs.map(_._1), gs.map(_._2), t.getOrElse(Expr.Lit(true)(pos)))(pos)
        }

  def stmt(s: Scope): Parser[Stmt] =
    skip | ifs(s) | loop(s) | call(s) | matches(s, body(s))(Stmt.Match(_, _)(_)) | check(s) | assign(s) | raise(s) |
      attempt(s)

  def check(s: Scope): Parser[Stmt] =
    located: pos =>
      kw("assert") ~> expr(s) ^^ (Stmt.Assert(_)(pos))

  def raise(s: Scope): Parser[Stmt] =
    located: pos =>
      kw("raise") ~> expr(s) ^^ (Stmt.Raise(_, Type.Hole)(pos))

  def attempt(s: Scope): Parser[Stmt] =
    located: pos =>
      (kw("try") ~> body(s)) ~
        (kw("catch") ~> rep(kw("case") ~> pattern(s) ~ ("=>" ~> body(s))) <~ kw("end")) ^^ { case b ~ hs =>
          Stmt.Try(
            b,
            hs.map { case p ~ c =>
              Handler(Type.Hole, p, c)
            }
          )(pos)
        }

  val skip: Parser[Stmt] = located(pos => kw("skip") ^^^ Stmt.Skip()(pos))

  def assign(s: Scope): Parser[Stmt] =
    located: pos =>
      kw("assign") ~> rep1sep(variable(s) ~ (":=" ~> expr(s)), ",") ^^ { bs =>
        Stmt.Assign(bs.map { case x ~ e =>
          (x, e)
        })(pos)
      }

  // the bodies start with a keyword, which ends the condition, and an if after else ends the whole chain
  def ifs(s: Scope): Parser[Stmt] =
    located: pos =>
      (kw("if") ~> expr(s)) ~
        body(s) ~ otherwise(s) ^^ { case c ~ l ~ r =>
          Stmt.If(c, l, r)(pos)
        }

  def otherwise(s: Scope): Parser[Stmt] =
    kw("else") ~> (ifs(s) | body(s) <~ kw("end")) | kw("end") ^^^ Stmt.Skip()(nowhere)

  def loop(s: Scope): Parser[Stmt] =
    located: pos =>
      (kw("while") ~> expr(s)) ~
        (body(s) <~ kw("end")) ^^ { case c ~ b =>
          Stmt.While(c, b)(pos)
        }

  def call(s: Scope): Parser[Stmt] =
    located: pos =>
      (kw("call") ~> name) ~ parens(sections(expr(s), variable(s))) ^^ { case p ~ ((ins, outs, refs)) =>
        Stmt.Call(p, ins, outs, refs)(pos)
      }

  // the body of an if, while, case, or procedure, which may declare variables first
  def body(s: Scope): Parser[Stmt] =
    located: pos =>
      rep(locals(s)) ~ rep(stmt(s)) ^^ { case ls ~ ss =>
        Stmt.Block(ls.flatten, ss)(pos)
      }

  def locals(s: Scope): Parser[List[Pat.Var]] =
    kw("local") ~> rep1sep(formal(s), ",")

  // a declaration that starts with keyword, name, and type parameters
  def generic(keyword: String, name: Parser[Name])(rest: (Name, List[Name], Scope) => Parser[Cmd]): Parser[Cmd] =
    (kw(keyword) ~> name) ~ typarams >> { case n ~ ps =>
      rest(n, ps, Scope(ps.toSet, Nil))
    }

  def notation(s: Scope, pos: Pos): Parser[Cmd] =
    val fixity =
      kw("prefix") ^^^ Fixity.Prefix | kw("postfix") ^^^ Fixity.Postfix |
        kw("infix") ~>
        (kw("left") ^^^ Fixity.Left | kw("right") ^^^ Fixity.Right | success(Fixity.Infix))
    fixity ~
      ("""\d+""".r ^^ (_.toInt)) ~ rep1(symbol | name) ^^ { case f ~ p ~ ns =>
        Cmd.Notation(f, p, ns)(pos)
      }

  def sort(s: Scope, pos: Pos): Parser[Cmd] =
    generic("sort", name): (n, ps, t) =>
      // the type may be preceded by |, like the constructors of a datatype
      ":=" ~> opt("|") ~> typ(t.copy(notations = s.notations)) ^^ (Cmd.Sort(n, ps, _)(pos))

  def data(s: Scope, pos: Pos): Parser[Cmd] =
    generic("data", name): (n, ps, t) =>

      val u = t.copy(notations = s.notations)
      val constr =
        (name | symbol) ~ opt(parens(list(typ(u)))) ^^ { case c ~ ts =>
          Constr(c, ts.getOrElse(Nil))
        }
      // the first constructor may be preceded by | as well
      ":=" ~> opt("|") ~> rep1sep(constr, "|") ^^ (Cmd.Data(n, ps, _)(pos))

  def fun(s: Scope, pos: Pos): Parser[Cmd] =
    generic("function", name | symbol): (n, ps, t) =>

      val u = t.copy(notations = s.notations)
      parens(formals(u)) ~
        (":" ~> typ(u)) ~
        (":=" ~> expr(u)) ^^ { case xs ~ r ~ b =>
          Cmd.Fun(n, ps, xs, r, b)(pos)
        }

  def proc(s: Scope, pos: Pos): Parser[Cmd] =
    generic("procedure", name): (n, ps, t) =>

      val u = t.copy(notations = s.notations)
      parens(sections(formal(u), formal(u))) ~
        (":=" ~> body(u)) ^^ { case (ins, outs, refs) ~ b =>
          Cmd.Proc(n, ps, ins, outs, refs, b)(pos)
        }

  def command(s: Scope): Parser[List[Cmd]] =
    located(pos => kw("import") ~> name ^^ (load(s, _, pos))) | declaration(s) ^^ (List(_))

  // inlines module n, after the marker Import(n), unless it is loaded already
  def load(s: Scope, n: Name, pos: Pos): List[Cmd] =
    if s.loaded(n) then
      Nil
    else
      val text = s.read(n)
      val cmds =
        try
          parse(commands(s.copy(loaded = s.loaded + n)), text)
        catch
          case e: ulang.Error =>
            throw new ulang.Error(s"${e.getMessage} in $n")
      Cmd.Import(n)(pos) :: cmds.filter(declares)

  // modules contribute their declarations, but not their evals, runs, and tests
  def declares(c: Cmd): Boolean =
    c match
      case Cmd.Eval(_) | Cmd.Run(_) | Cmd.Test(_, _) =>
        false
      case _ =>
        true

  // the bodies of commands end at the keyword of the next command, so commands need no end
  def declaration(s: Scope): Parser[Cmd] =
    located: pos =>
      notation(s, pos) | sort(s, pos) | data(s, pos) | fun(s, pos) | proc(s, pos) |
        kw("eval") ~> expr(s) ^^
        (Cmd.Eval(_)(pos)) |
        kw("run") ~> body(s) ^^
        (Cmd.Run(_)(pos)) |
        (kw("test") ~> name) ~ (":=" ~> body(s)) ^^ { case n ~ b =>
          Cmd.Test(n, b)(pos)
        }

  // notations apply to the commands after them
  def commands(s: Scope): Parser[List[Cmd]] =
    command(s) >> (cs => commands(cs.foldLeft(s)(declare)) ^^ (cs ++ _)) | success(Nil)

  def declare(s: Scope, c: Cmd): Scope =
    c match
      case n: Cmd.Notation =>
        s.copy(notations = s.notations :+ n)
      case Cmd.Import(n) =>
        s.copy(loaded = s.loaded + n)
      case _ =>
        s

  def imports(p: Program): Set[Name] =
    p.cmds.collect { case Cmd.Import(n) =>
      n
    }.toSet

  def notations(p: Program): List[Cmd.Notation] =
    p.cmds.collect:
      case n: Cmd.Notation =>
        n

  def parse[A](p: Parser[A], text: String): A =
    parseAll(p, text) match
      case Success(a, _) =>
        a
      case failure: NoSuccess =>
        error("parse", failure.msg, failure.next.pos)

  // the program text with the notations of a preceding program, e.g., the prelude
  def parseProgram(text: String, notations: List[Cmd.Notation], loaded: Set[Name] = Set(), read: Name => String = Scope.none): Program =
    Program(parse(commands(Scope(Set(), notations, loaded, read)), text))

// precedence climbing, where prefix operators bind their operand with their own precedence
case class Resolve[A](s: Scope, prefix: (Name, A) => A, postfix: (Name, A) => A, infix: (Name, A, A) => A):
  def fail(msg: String, at: Option[scala.util.parsing.input.Reader[Char]] = None): Nothing =
    throw Unresolved(msg, at)

  def all(items: List[Item[A]]): A =
    climb(items, 0) match
      case (a, Nil) =>
        a
      case (_, Item.Operator(op, at) :: _) =>
        fail(s"unexpected operator $op", Some(at))
      case (_, _) =>
        fail("missing operator")

  def climb(items: List[Item[A]], min: Int): (A, List[Item[A]]) =
    val (lhs, rest) = operand(items)
    loop(lhs, rest, min)

  def operand(items: List[Item[A]]): (A, List[Item[A]]) =
    items match
      case Item.Operand(a) :: rest =>
        (a, rest)
      case Item.Operator(op, _) :: rest if s.prefix(op).nonEmpty =>
        val (a, rest1) = climb(rest, s.prefix(op).get)
        (prefix(op, a), rest1)
      case Item.Operator(op, at) :: _ if s.declared(op) =>
        fail(s"$op is not a prefix operator", Some(at))
      case Item.Operator(op, at) :: _ =>
        fail(s"undeclared operator $op", Some(at))
      case Nil =>
        fail("missing operand")

  def loop(lhs: A, items: List[Item[A]], min: Int): (A, List[Item[A]]) =
    items match
      case Item.Operator(op, _) :: rest if s.postfix(op).exists(_ >= min) =>
        loop(postfix(op, lhs), rest, min)
      case Item.Operator(op, _) :: rest if s.infix(op).exists(_._2 >= min) =>
        val (fixity, prec) = s.infix(op).get
        val (rhs, rest1) = climb(
          rest,
          if fixity == Fixity.Right then
            prec
          else
            prec + 1
        )
        nonassociative(op, fixity, prec, rest1)
        loop(infix(op, lhs, rhs), rest1, min)
      case Item.Operator(op, at) :: _ if !s.declared(op) =>
        fail(s"undeclared operator $op", Some(at))
      case _ =>
        (lhs, items)

  def nonassociative(op: Name, fixity: Fixity, prec: Int, rest: List[Item[A]]): Unit =
    rest match
      case Item.Operator(next, at) :: _ if fixity == Fixity.Infix && s.infix(next).exists(_._2 == prec) =>
        fail(s"operator $op is not associative", Some(at))
      case _ =>
