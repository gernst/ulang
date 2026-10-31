package ulang

object Parser extends scala.util.parsing.combinator.RegexParsers:
  override val whiteSpace = """(\s|//.*)+""".r

  val keywords = Set(
    "_",
    "and",
    "assert",
    "call",
    "case",
    "data",
    "do",
    "else",
    "end",
    "eval",
    "false",
    "fun",
    "function",
    "if",
    "in",
    "let",
    "match",
    "not",
    "or",
    "procedure",
    "run",
    "skip",
    "sort",
    "test",
    "then",
    "true",
    "var",
    "while"
  )

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

  val names: Parser[List[Name]] = rep1sep(name, ",")

  def list[A](p: => Parser[A]): Parser[List[A]] =
    repsep(p, ",")

  def parens[A](p: => Parser[A]): Parser[A] =
    "(" ~> p <~ ")"

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

  // ps are the type parameters in scope, which distinguish Param from Sort
  def typ(ps: Set[Name]): Parser[Type] =
    (parens(list(typ(ps))) ~ ("->" ~> typ(ps))) ^^ { case as ~ r =>
      Type.Sort("->", as :+ r)
    } |
      basic(ps) ~ opt("->" ~> typ(ps)) ^^ {
        case a ~ None =>
          a
        case a ~ Some(r) =>
          Type.Sort("->", List(a, r))
      }

  def basic(ps: Set[Name]): Parser[Type] =
    name ~ opt("[" ~> rep1sep(typ(ps), ",") <~ "]") ^^ {
      case n ~ None if ps(n) =>
        Type.Param(n)
      case n ~ args =>
        Type.Sort(n, args.getOrElse(Nil))
    } | tuple(typ(ps))((a, b) => Type.Sort("*", List(a, b)))

  val typarams: Parser[List[Name]] = opt("[" ~> names <~ "]") ^^ (_.getOrElse(Nil))

  val variable: Parser[Expr.Var] = name ^^ (Expr.Var(_, Type.Hole))

  def formal(ps: Set[Name]): Parser[Pat.Var] =
    name ~ opt(":" ~> typ(ps)) ^^ { case x ~ t =>
      Pat.Var(x, t.getOrElse(Type.Hole))
    }

  def formals(ps: Set[Name]): Parser[List[Pat.Var]] =
    list(formal(ps))

  def expr(ps: Set[Name]): Parser[Expr] =
    let(ps) | ite(ps) | lam(ps) | matches(ps, expr(ps))(Expr.Match.apply) | or(ps)

  def exprs(ps: Set[Name]): Parser[List[Expr]] =
    rep1sep(expr(ps), ",")

  // a variable, possibly with a type, or a pattern
  def binder(ps: Set[Name]): Parser[Pat] =
    formal(ps) <~ not("(") | pattern

  // variables are bound in parallel, other patterns are matched against the tuple of values
  def let(ps: Set[Name]): Parser[Expr] =
    (kw("let") ~> rep1sep(binder(ps), ",")) ~
      ("=" ~> exprs(ps)) ~
      (kw("in") ~> expr(ps)) ^? (
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

  def ite(ps: Set[Name]): Parser[Expr] =
    (kw("if") ~> expr(ps)) ~
      (kw("then") ~> expr(ps)) ~
      (kw("else") ~> expr(ps)) ^^ { case c ~ l ~ r =>
        Expr.Ite(c, l, r)
      }

  def lam(ps: Set[Name]): Parser[Expr] =
    (kw("fun") ~> parens(formals(ps))) ~
      ("=>" ~> expr(ps)) ^^ { case xs ~ b =>
        Expr.Lam(xs, b)
      }

  def matches[A, B](ps: Set[Name], body: => Parser[A])(make: (Expr, List[Case[A]]) => B): Parser[B] =
    (kw("match") ~> expr(ps)) ~
      (rep(kw("case") ~> pattern ~ ("=>" ~> body)) <~ kw("end")) ^^ { case e ~ cs =>
        make(
          e,
          cs.map { case p ~ b =>
            Case(p, b)
          }
        )
      }

  lazy val pattern: Parser[Pat] =
    name ~ parens(list(pattern)) ^^ { case c ~ ps =>
      Pat.App(c, ps)
    } | value ^^ Pat.Lit.apply | tuple(pattern)((a, b) => Pat.App(",", List(a, b))) | kw("_") ^^^ Pat.Wild |
      name ^^
      (Pat.Var(_, Type.Hole))

  def op(f: Name, args: Expr*): Expr =
    Expr.App(Expr.Var(f, Type.Hole), args.toList)

  def binary(ops: Parser[String], arg: Parser[Expr]): Parser[Expr] =
    chainl1(arg, ops ^^ (f => (l: Expr, r: Expr) => op(f, l, r)))

  def or(ps: Set[Name]): Parser[Expr] =
    binary(kw("or"), and(ps))
  def and(ps: Set[Name]): Parser[Expr] =
    binary(kw("and"), negation(ps))

  def negation(ps: Set[Name]): Parser[Expr] =
    kw("not") ~> negation(ps) ^^ (op("not", _)) | cmp(ps)

  def cmp(ps: Set[Name]): Parser[Expr] =
    binary("==" | "!=" | "<=" | "<" | ">=" | ">", add(ps))
  def add(ps: Set[Name]): Parser[Expr] =
    binary("+" | "-", mul(ps))
  def mul(ps: Set[Name]): Parser[Expr] =
    binary("*" | "/" | "%", neg(ps))

  def neg(ps: Set[Name]): Parser[Expr] =
    "-" ~> neg(ps) ^^ (op("uminus", _)) | app(ps)

  def app(ps: Set[Name]): Parser[Expr] =
    atom(ps) ~ rep(parens(list(expr(ps)))) ^^ { case f ~ argss =>
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

  def atom(ps: Set[Name]): Parser[Expr] =
    value ^^ Expr.Lit.apply | tuple(expr(ps))(op(",", _, _)) | variable

  def stmt(ps: Set[Name]): Parser[Stmt] =
    skip | ifs(ps) | loop(ps) | call(ps) | matches(ps, stmts(ps))(Stmt.Match.apply) | check(ps) | assign(ps)

  def check(ps: Set[Name]): Parser[Stmt] =
    kw("assert") ~> expr(ps) <~ ";" ^^ Stmt.Assert.apply

  // the body of a case, which declares no variables
  def stmts(ps: Set[Name]): Parser[Stmt] =
    rep(stmt(ps)) ^^ (Stmt.Block(Nil, _))

  val skip: Parser[Stmt] = kw("skip") ~ ";" ^^^ Stmt.Skip

  def assign(ps: Set[Name]): Parser[Stmt] =
    rep1sep(variable, ",") ~
      (":=" ~> exprs(ps)) <~ ";" ^^ { case xs ~ es =>
        Stmt.Assign(xs, es)
      }

  // an if after else ends the whole chain
  def ifs(ps: Set[Name]): Parser[Stmt] =
    (kw("if") ~> expr(ps)) ~
      (kw("then") ~> body(ps)) ~ otherwise(ps) ^^ { case c ~ l ~ r =>
        Stmt.If(c, l, r)
      }

  def otherwise(ps: Set[Name]): Parser[Stmt] =
    kw("else") ~> (ifs(ps) | body(ps) <~ kw("end")) | kw("end") ^^^ Stmt.Skip

  def loop(ps: Set[Name]): Parser[Stmt] =
    (kw("while") ~> expr(ps)) ~
      (kw("do") ~> body(ps) <~ kw("end")) ^^ { case c ~ b =>
        Stmt.While(c, b)
      }

  def call(ps: Set[Name]): Parser[Stmt] =
    (kw("call") ~> name) ~ parens(sections(list(expr(ps)), list(variable))) <~ ";" ^^ { case p ~ ((ins, outs, refs)) =>
      Stmt.Call(p, ins, outs, refs)
    }

  // the body of an if, while, or procedure, which may declare variables first
  def body(ps: Set[Name]): Parser[Stmt] =
    rep(locals(ps)) ~ rep(stmt(ps)) ^^ { case ls ~ ss =>
      Stmt.Block(ls.flatten, ss)
    }

  def locals(ps: Set[Name]): Parser[List[Pat.Var]] =
    kw("var") ~> rep1sep(formal(ps), ",") <~ ";"

  // a declaration that starts with keyword, name, and type parameters
  def generic(keyword: String)(rest: (Name, List[Name], Set[Name]) => Parser[Cmd]): Parser[Cmd] =
    (kw(keyword) ~> name) ~ typarams >> { case n ~ ps =>
      rest(n, ps, ps.toSet)
    }

  val sort: Parser[Cmd] =
    generic("sort"): (n, ps, s) =>
      "=" ~> typ(s) ^^ (Cmd.Sort(n, ps, _))

  val data: Parser[Cmd] =
    generic("data"): (n, ps, s) =>

      val constr =
        name ~ opt(parens(list(typ(s)))) ^^ { case c ~ ts =>
          Constr(c, ts.getOrElse(Nil))
        }
      "=" ~> rep1sep(constr, "|") ^^ (Cmd.Data(n, ps, _))

  val fun: Parser[Cmd] =
    generic("function"): (n, ps, s) =>
      parens(formals(s)) ~
        (":" ~> typ(s)) ~
        ("=" ~> expr(s)) ^^ { case xs ~ t ~ b =>
          Cmd.Fun(n, ps, xs, t, b)
        }

  val proc: Parser[Cmd] =
    generic("procedure"): (n, ps, s) =>
      parens(sections(formals(s), formals(s))) ~
        (body(s) <~ kw("end")) ^^ { case (ins, outs, refs) ~ b =>
          Cmd.Proc(n, ps, ins, outs, refs, b)
        }

  val eval: Parser[Cmd] = kw("eval") ~> expr(Set()) ^^ Cmd.Eval.apply

  val run: Parser[Cmd] = kw("run") ~> body(Set()) <~ kw("end") ^^ Cmd.Run.apply

  val test: Parser[Cmd] = kw("test") ~> body(Set()) <~ kw("end") ^^ Cmd.Test.apply

  val program: Parser[Program] = rep(sort | data | fun | proc | eval | run | test) ^^ Program.apply

  def parse[A](p: Parser[A], text: String): A =
    parseAll(p, text) match
      case Success(a, _) =>
        a
      case failure: NoSuccess =>
        throw new ulang.Error(s"parse error: ${failure.msg} at ${failure.next.pos}")

  def parseType(text: String): Type =
    parse(typ(Set()), text)
  def parseExpr(text: String): Expr =
    parse(expr(Set()), text)
  def parseStmt(text: String): Stmt =
    parse(stmt(Set()), text)
  def parseProgram(text: String): Program =
    parse(program, text)
