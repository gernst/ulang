package ulang

// params are the type parameters in scope, which distinguish Param from Sort,
// notations are the operators declared so far, loaded are the modules imported so far,
// and read gives the text of a module
case class Scope(
    params: Set[Name],
    notations: List[Cmd.Notation],
    loaded: Set[Name] = Set(),
    read: Name => String = Scope.none
):
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
    throw new ulang.Error(s"parse error: unknown module $name")

// operands and operators of an expression or type, before their precedence is resolved
enum Item[A]:
  case Operand(a: A)
  case Operator(name: Name)

object Parser extends scala.util.parsing.combinator.RegexParsers:
  override val whiteSpace = """(\s|//.*)+""".r

  val keywords = Set(
    "_",
    "assert",
    "call",
    "case",
    "data",
    "do",
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
    "postfix",
    "prefix",
    "procedure",
    "run",
    "skip",
    "sort",
    "test",
    "then",
    "true",
    "while"
  )

  // symbols with a fixed meaning, which cannot be operators
  val reserved = Set("=", "=>", ":=", ":", "|")

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
    symbol | name ^? {
      case x if s.declared(x) =>
        x
    }

  val names: Parser[List[Name]] = rep1sep(name, ",")

  def list[A](p: => Parser[A]): Parser[List[A]] =
    repsep(p, ",")

  def parens[A](p: => Parser[A]): Parser[A] =
    "(" ~> p <~ ")"

  def pair[A](items: List[A])(pair: (A, A) => A): A =
    items.reduceRight(pair)

  // (a) is just a, and (a, b, c) is (a, (b, c)), since tuples are binary
  def tuple[A](p: => Parser[A])(pair: (A, A) => A): Parser[A] =
    parens(rep1sep(p, ",")) ^^ (_.reduceRight(pair))

  // ins; outs; refs, where trailing empty sections may be omitted
  def sections[A, B](ins: Parser[List[A]], rest: Parser[List[B]]): Parser[(List[A], List[B], List[B])] =
    ins ~ opt(";" ~> rest ~ opt(";" ~> rest)) ^^ {
      case as ~ None =>
        (as, Nil, Nil)
      case as ~ Some(bs ~ cs) =>
        (as, bs, cs.getOrElse(Nil))
    }

  // a sequence of operands and operators
  def items[A](s: Scope, operand: => Parser[A]): Parser[List[Item[A]]] =
    rep1(operand ^^ (Item.Operand(_)) | operator(s) ^^ (Item.Operator[A](_)))

  // resolves the precedence of operators by precedence climbing, failing with a message
  def resolved[A](s: Scope, items: Parser[List[Item[A]]])(
      prefix: (Name, A) => A,
      postfix: (Name, A) => A,
      infix: (Name, A, A) => A
  ): Parser[A] =
    items >> { is =>
      try
        success(Resolve(s, prefix, postfix, infix).all(is))
      catch
        case e: ulang.Error =>
          failure(e.getMessage)
    }

  def typ(s: Scope): Parser[Type] =
    def single(ts: List[Type]): Type =
      pair(ts)((a, b) => Type.Sort("*", List(a, b)))
    resolved(s, items(s, basic(s)))(
      (op, t) => List(Type.Sort(op, List(single(t)))),
      (op, t) => List(Type.Sort(op, List(single(t)))),
      // for ->, a parenthesized list on the left are the arguments of a function
      (op, l, r) =>
        if op == "->"
        then List(Type.Sort("->", l :+ single(r)))
        else List(Type.Sort(op, List(single(l), single(r))))
    ) ^^ single

  // a type, or a parenthesized list of types
  def basic(s: Scope): Parser[List[Type]] =
    plain(s) ~ opt("[" ~> rep1sep(typ(s), ",") <~ "]") ^^ {
      case n ~ None if s.params(n) =>
        List(Type.Param(n))
      case n ~ args =>
        List(Type.Sort(n, args.getOrElse(Nil)))
    } | parens(list(typ(s)))

  val typarams: Parser[List[Name]] = opt("[" ~> names <~ "]") ^^ (_.getOrElse(Nil))

  def variable(s: Scope): Parser[Expr.Var] =
    plain(s) ^^ (Expr.Var(_, Type.Hole))

  // only parameters and locals have type annotations
  def untyped(s: Scope): Parser[Pat.Var] =
    plain(s) ^^ (Pat.Var(_, Type.Hole))

  def formal(s: Scope): Parser[Pat.Var] =
    plain(s) ~ opt(":" ~> typ(s)) ^^ { case x ~ t =>
      Pat.Var(x, t.getOrElse(Type.Hole))
    }

  def formals(s: Scope): Parser[List[Pat.Var]] =
    list(formal(s))

  def expr(s: Scope): Parser[Expr] =
    let(s) | ite(s) | lam(s) | matches(s, expr(s))(Expr.Match.apply) | operators(s)

  def exprs(s: Scope): Parser[List[Expr]] =
    rep1sep(expr(s), ",")

  def operators(s: Scope): Parser[Expr] =
    resolved(s, items(s, app(s)))(op(_, _), op(_, _), op(_, _, _))

  // variables are bound in parallel, other patterns are matched against the tuple of values
  def let(s: Scope): Parser[Expr] =
    (kw("let") ~> rep1sep(pattern(s), ",")) ~
      ("=" ~> exprs(s)) ~
      (kw("in") ~> expr(s)) ^? (
        {
          case xs ~ es ~ b if xs.forall(_.isInstanceOf[Pat.Var]) =>
            Expr.Let(
              xs.collect { case x: Pat.Var =>
                x
              },
              es,
              b
            )
          case xs ~ es ~ b if xs.length == es.length =>
            Expr.Match(es.reduceRight(op(",", _, _)), List(Case(xs.reduceRight((a, b) => Pat.App(",", List(a, b))), b)))
        },
        { case xs ~ es ~ _ =>
          s"let expects ${xs.length} values, got ${es.length}"
        }
      )

  def ite(s: Scope): Parser[Expr] =
    (kw("if") ~> expr(s)) ~
      (kw("then") ~> expr(s)) ~
      (kw("else") ~> expr(s)) ^^ { case c ~ l ~ r =>
        Expr.Ite(c, l, r)
      }

  def lam(s: Scope): Parser[Expr] =
    (kw("lambda") ~> repsep(untyped(s), ",")) ~
      ("=>" ~> expr(s)) ^^ { case xs ~ b =>
        Expr.Lam(xs, b)
      }

  def matches[A, B](s: Scope, body: => Parser[A])(make: (Expr, List[Case[A]]) => B): Parser[B] =
    (kw("match") ~> expr(s)) ~
      (rep(kw("case") ~> pattern(s) ~ ("=>" ~> body)) <~ kw("end")) ^^ { case e ~ cs =>
        make(
          e,
          cs.map { case p ~ b =>
            Case(p, b)
          }
        )
      }

  // operators in patterns are constructors
  def pattern(s: Scope): Parser[Pat] =
    resolved(s, items(s, simple(s)))(
      (op, p) => Pat.App(op, List(p)),
      (op, p) => Pat.App(op, List(p)),
      (op, l, r) => Pat.App(op, List(l, r))
    )

  def simple(s: Scope): Parser[Pat] =
    plain(s) ~ parens(list(pattern(s))) ^^ { case c ~ ps =>
      Pat.App(c, ps)
    }
      | value ^^ Pat.Lit.apply
      | tuple(pattern(s))((a, b) => Pat.App(",", List(a, b)))
      | kw("_") ^^^ Pat.Wild
      | plain(s) ^^ (Pat.Var(_, Type.Hole))

  def op(f: Name, args: Expr*): Expr =
    Expr.App(Expr.Var(f, Type.Hole), args.toList)

  def app(s: Scope): Parser[Expr] =
    atom(s) ~ rep(parens(list(expr(s)))) ^^ { case f ~ argss =>
      argss.foldLeft(f)(Expr.App(_, _))
    }

  val value: Parser[Any] =
    kw("true") ^^^ true | kw("false") ^^^ false |
      """\d+""".r ^^
      (BigInt(_)) |
      "\"[^\"]*\"".r ^^
      (s => s.substring(1, s.length - 1)) |
      "(" ~ ")" ^^^
      (())

  def atom(s: Scope): Parser[Expr] =
    value ^^ Expr.Lit.apply
      | tuple(expr(s))(op(",", _, _))
      | comp(s)
      | "[" ~> list(expr(s)) <~ "]" ^^ (_.foldRight(Expr.Var("nil", Type.Hole): Expr)(op("::", _, _)))
      | variable(s)

  // [result for x in xs, y in ys if test], where if test is optional
  def comp(s: Scope): Parser[Expr] =
    ("[" ~> expr(s)) ~
      (kw("for") ~> rep1sep(untyped(s) ~ (kw("in") ~> expr(s)), ",")) ~
      (opt(kw("if") ~> expr(s)) <~ "]") ^^ { case e ~ gs ~ t =>
        Expr.Comp(e, gs.map(_._1), gs.map(_._2), t.getOrElse(Expr.Lit(true)))
      }

  def stmt(s: Scope): Parser[Stmt] =
    skip | ifs(s) | loop(s) | call(s) | matches(s, stmts(s))(Stmt.Match.apply) | check(s) | assign(s)

  def check(s: Scope): Parser[Stmt] =
    kw("assert") ~> expr(s) <~ ";" ^^ Stmt.Assert.apply

  // the body of a case, which declares no variables
  def stmts(s: Scope): Parser[Stmt] =
    rep(stmt(s)) ^^ (Stmt.Block(Nil, _))

  val skip: Parser[Stmt] = kw("skip") ~ ";" ^^^ Stmt.Skip

  def assign(s: Scope): Parser[Stmt] =
    rep1sep(variable(s), ",") ~
      (":=" ~> exprs(s)) <~ ";" ^^ { case xs ~ es =>
        Stmt.Assign(xs, es)
      }

  // an if after else ends the whole chain
  def ifs(s: Scope): Parser[Stmt] =
    (kw("if") ~> expr(s)) ~
      (kw("then") ~> body(s)) ~ otherwise(s) ^^ { case c ~ l ~ r =>
        Stmt.If(c, l, r)
      }

  def otherwise(s: Scope): Parser[Stmt] =
    kw("else") ~> (ifs(s) | body(s) <~ kw("end")) | kw("end") ^^^ Stmt.Skip

  def loop(s: Scope): Parser[Stmt] =
    (kw("while") ~> expr(s)) ~
      (kw("do") ~> body(s) <~ kw("end")) ^^ { case c ~ b =>
        Stmt.While(c, b)
      }

  def call(s: Scope): Parser[Stmt] =
    (kw("call") ~> name) ~ parens(sections(list(expr(s)), list(variable(s)))) <~ ";" ^^ { case p ~ ((ins, outs, refs)) =>
      Stmt.Call(p, ins, outs, refs)
    }

  // the body of an if, while, or procedure, which may declare variables first
  def body(s: Scope): Parser[Stmt] =
    rep(locals(s)) ~ rep(stmt(s)) ^^ { case ls ~ ss =>
      Stmt.Block(ls.flatten, ss)
    }

  def locals(s: Scope): Parser[List[Pat.Var]] =
    kw("local") ~> rep1sep(formal(s), ",") <~ ";"

  // a declaration that starts with keyword, name, and type parameters
  def generic(keyword: String, name: Parser[Name])(rest: (Name, List[Name], Scope) => Parser[Cmd]): Parser[Cmd] =
    (kw(keyword) ~> name) ~ typarams >> { case n ~ ps =>
      rest(n, ps, Scope(ps.toSet, Nil))
    }

  def notation(s: Scope): Parser[Cmd] =
    val fixity =
      kw("prefix") ^^^ Fixity.Prefix
        | kw("postfix") ^^^ Fixity.Postfix
        | kw("infix") ~> (kw("left") ^^^ Fixity.Left | kw("right") ^^^ Fixity.Right | success(Fixity.Infix))
    fixity ~ ("""\d+""".r ^^ (_.toInt)) ~ rep1(symbol | name) ^^ { case f ~ p ~ ns =>
      Cmd.Notation(f, p, ns)
    }

  def sort(s: Scope): Parser[Cmd] =
    generic("sort", name): (n, ps, t) =>
      "=" ~> typ(t.copy(notations = s.notations)) ^^ (Cmd.Sort(n, ps, _))

  def data(s: Scope): Parser[Cmd] =
    generic("data", name): (n, ps, t) =>
      val u = t.copy(notations = s.notations)
      val constr =
        (name | symbol) ~ opt(parens(list(typ(u)))) ^^ { case c ~ ts =>
          Constr(c, ts.getOrElse(Nil))
        }
      "=" ~> rep1sep(constr, "|") ^^ (Cmd.Data(n, ps, _))

  def fun(s: Scope): Parser[Cmd] =
    generic("function", name | symbol): (n, ps, t) =>
      val u = t.copy(notations = s.notations)
      parens(formals(u)) ~
        (":" ~> typ(u)) ~
        ("=" ~> expr(u)) ^^ { case xs ~ r ~ b =>
          Cmd.Fun(n, ps, xs, r, b)
        }

  def proc(s: Scope): Parser[Cmd] =
    generic("procedure", name): (n, ps, t) =>
      val u = t.copy(notations = s.notations)
      parens(sections(formals(u), formals(u))) ~
        (body(u) <~ kw("end")) ^^ { case (ins, outs, refs) ~ b =>
          Cmd.Proc(n, ps, ins, outs, refs, b)
        }

  def command(s: Scope): Parser[List[Cmd]] =
    kw("import") ~> name ^^ (load(s, _)) | declaration(s) ^^ (List(_))

  // inlines module n, after the marker Import(n), unless it is loaded already
  def load(s: Scope, n: Name): List[Cmd] =
    if s.loaded(n)
    then Nil
    else
      val text = s.read(n)
      val cmds =
        try parse(commands(s.copy(loaded = s.loaded + n)), text)
        catch
          case e: ulang.Error =>
            throw new ulang.Error(s"${e.getMessage} in $n")
      Cmd.Import(n) :: cmds

  def declaration(s: Scope): Parser[Cmd] =
    notation(s)
      | sort(s)
      | data(s)
      | fun(s)
      | proc(s)
      | kw("eval") ~> expr(s) ^^ Cmd.Eval.apply
      | kw("run") ~> body(s) <~ kw("end") ^^ Cmd.Run.apply
      | kw("test") ~> body(s) <~ kw("end") ^^ Cmd.Test.apply

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
        throw new ulang.Error(s"parse error: ${failure.msg} at ${failure.next.pos}")

  // the program text with the notations of a preceding program, e.g., the prelude
  def parseProgram(text: String, notations: List[Cmd.Notation], loaded: Set[Name] = Set(), read: Name => String = Scope.none): Program =
    Program(parse(commands(Scope(Set(), notations, loaded, read)), text))

// precedence climbing, where prefix operators bind their operand with their own precedence
case class Resolve[A](s: Scope, prefix: (Name, A) => A, postfix: (Name, A) => A, infix: (Name, A, A) => A):
  def fail(msg: String): Nothing =
    throw new ulang.Error(msg)

  def all(items: List[Item[A]]): A =
    climb(items, 0) match
      case (a, Nil) =>
        a
      case (_, Item.Operator(op) :: _) =>
        fail(s"unexpected operator $op")
      case (_, _) =>
        fail("missing operator")

  def climb(items: List[Item[A]], min: Int): (A, List[Item[A]]) =
    val (lhs, rest) = operand(items)
    loop(lhs, rest, min)

  def operand(items: List[Item[A]]): (A, List[Item[A]]) =
    items match
      case Item.Operand(a) :: rest =>
        (a, rest)
      case Item.Operator(op) :: rest if s.prefix(op).nonEmpty =>
        val (a, rest1) = climb(rest, s.prefix(op).get)
        (prefix(op, a), rest1)
      case Item.Operator(op) :: _ if s.declared(op) =>
        fail(s"$op is not a prefix operator")
      case Item.Operator(op) :: _ =>
        fail(s"undeclared operator $op")
      case Nil =>
        fail("missing operand")

  def loop(lhs: A, items: List[Item[A]], min: Int): (A, List[Item[A]]) =
    items match
      case Item.Operator(op) :: rest if s.postfix(op).exists(_ >= min) =>
        loop(postfix(op, lhs), rest, min)
      case Item.Operator(op) :: rest if s.infix(op).exists(_._2 >= min) =>
        val (fixity, prec) = s.infix(op).get
        val (rhs, rest1) = climb(rest, if fixity == Fixity.Right then prec else prec + 1)
        nonassociative(op, fixity, prec, rest1)
        loop(infix(op, lhs, rhs), rest1, min)
      case Item.Operator(op) :: _ if !s.declared(op) =>
        fail(s"undeclared operator $op")
      case _ =>
        (lhs, items)

  def nonassociative(op: Name, fixity: Fixity, prec: Int, rest: List[Item[A]]): Unit =
    rest match
      case Item.Operator(next) :: _ if fixity == Fixity.Infix && s.infix(next).exists(_._2 == prec) =>
        fail(s"operator $op is not associative")
      case _ =>
