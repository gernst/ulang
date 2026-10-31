package ulang
package source

// parses without context: operators remain unresolved, and imports are not loaded
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

  // the position of the next token
  val at: Parser[Pos] =
    Parser(in => Success(in.drop(handleWhiteSpace(in.source, in.offset) - in.offset).pos, in))

  // a node at the position of its first token
  def located[A](p: => Parser[Pos => A]): Parser[A] =
    at ~ p ^^ { case pos ~ f =>
      f(pos)
    }

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

  val names: Parser[List[Name]] = rep1sep(name, ",")

  def list[A](p: => Parser[A]): Parser[List[A]] =
    repsep(p, ",")

  def parens[A](p: => Parser[A]): Parser[A] =
    "(" ~> p <~ ")"

  // ins; outs; refs, where trailing empty sections may be omitted
  def sections[A, B](ins: Parser[List[A]], rest: Parser[List[B]]): Parser[(List[A], List[B], List[B])] =
    ins ~ opt(";" ~> rest ~ opt(";" ~> rest)) ^^ {
      case as ~ None =>
        (as, Nil, Nil)
      case as ~ Some(bs ~ cs) =>
        (as, bs, cs.getOrElse(Nil))
    }

  // a sequence of operands and symbolic operators, names may turn out to be operators, too
  def items[A](operand: => Parser[A]): Parser[List[Item[A]]] =
    rep1(operand ^^ (Item.Arg(_)) | at ~ symbol ^^ { case p ~ op =>
      Item.Op[A](op, p)
    })

  def typ: Parser[Type] =
    located(items(basic) ^^ (is => Type.Ops(is, _)))

  def basic: Parser[Type] =
    located(
      name ~ opt("[" ~> rep1sep(typ, ",") <~ "]") ^^ { case n ~ args =>
        Type.Sort(n, args.getOrElse(Nil), _)
      }
        | parens(list(typ)) ^^ (ts => Type.Tuple(ts, _))
    )

  val typarams: Parser[List[Name]] = opt("[" ~> names <~ "]") ^^ (_.getOrElse(Nil))

  def variable: Parser[Expr.Var] =
    at ~ name ^^ { case p ~ x =>
      Expr.Var(x, p)
    }

  // a variable, possibly with a type
  def formal: Parser[Pat.Var] =
    at ~ name ~ opt(":" ~> typ) ^^ { case p ~ x ~ t =>
      Pat.Var(x, t, p)
    }

  def untyped: Parser[Pat.Var] =
    at ~ name ^^ { case p ~ x =>
      Pat.Var(x, None, p)
    }

  def formals: Parser[List[Pat.Var]] =
    list(formal)

  def expr: Parser[Expr] =
    let | ite | lam | matches(expr)((e, cs) => Expr.Match(e, cs, _)) | operators

  def exprs: Parser[List[Expr]] =
    rep1sep(expr, ",")

  def operators: Parser[Expr] =
    located(items(app) ^^ (is => Expr.Ops(is, _)))

  def let: Parser[Expr] =
    located((kw("let") ~> rep1sep(pattern, ",")) ~ ("=" ~> exprs) ~ (kw("in") ~> expr) ^^ { case xs ~ es ~ b =>
      Expr.Let(xs, es, b, _)
    })

  def ite: Parser[Expr] =
    located((kw("if") ~> expr) ~ (kw("then") ~> expr) ~ (kw("else") ~> expr) ^^ { case c ~ l ~ r =>
      Expr.Ite(c, l, r, _)
    })

  def lam: Parser[Expr] =
    located((kw("lambda") ~> repsep(untyped, ",")) ~ ("=>" ~> expr) ^^ { case xs ~ b =>
      Expr.Lam(xs, b, _)
    })

  def matches[A, B](body: => Parser[A])(make: (Expr, List[Case[A]]) => Pos => B): Parser[B] =
    located((kw("match") ~> expr) ~ (rep(located(kw("case") ~> pattern ~ ("=>" ~> body) ^^ { case p ~ b =>
      Case(p, b, _)
    })) <~ kw("end")) ^^ { case e ~ cs =>
      make(e, cs)
    })

  def pattern: Parser[Pat] =
    located(items(simple) ^^ (is => Pat.Ops(is, _)))

  def simple: Parser[Pat] =
    located(
      name ~ parens(list(pattern)) ^^ { case c ~ ps =>
        Pat.App(c, ps, _)
      }
        | value ^^ (v => Pat.Lit(v, _))
        | parens(rep1sep(pattern, ",")) ^^ (ps => Pat.Tuple(ps, _))
        | kw("_") ^^^ (Pat.Wild(_))
        | name ^^ (x => Pat.Var(x, None, _))
    )

  def app: Parser[Expr] =
    located(atom ~ rep(parens(list(expr))) ^^ { case f ~ argss =>
      p => argss.foldLeft(f)(Expr.App(_, _, p))
    })

  val value: Parser[Any] =
    kw("true") ^^^ true | kw("false") ^^^ false |
      """\d+""".r ^^
      (BigInt(_)) |
      "\"[^\"]*\"".r ^^
      (s => s.substring(1, s.length - 1)) |
      "(" ~ ")" ^^^
      (())

  def atom: Parser[Expr] =
    located(
      value ^^ (v => Expr.Lit(v, _))
        | parens(rep1sep(expr, ",")) ^^ (es => Expr.Tuple(es, _))
        | comp
        | "[" ~> list(expr) <~ "]" ^^ (es => Expr.Elems(es, _))
        | name ^^ (x => Expr.Var(x, _))
    )

  // [result for x in xs, y in ys if test], where if test is optional
  def comp: Parser[Pos => Expr] =
    ("[" ~> expr) ~ (kw("for") ~> rep1sep(untyped ~ (kw("in") ~> expr), ",")) ~ (opt(kw("if") ~> expr) <~ "]") ^^ {
      case e ~ gs ~ t =>
        Expr.Comp(e, gs.map(_._1), gs.map(_._2), t, _)
    }

  def stmt: Parser[Stmt] =
    skip | ifs | loop | call | matches(stmts)((e, cs) => Stmt.Match(e, cs, _)) | check | assign

  def check: Parser[Stmt] =
    located(kw("assert") ~> expr <~ ";" ^^ (c => Stmt.Assert(c, _)))

  // the body of a case, which declares no variables
  def stmts: Parser[Stmt] =
    located(rep(stmt) ^^ (ss => Stmt.Block(Nil, ss, _)))

  def skip: Parser[Stmt] =
    located(kw("skip") ~ ";" ^^^ (Stmt.Skip(_)))

  def assign: Parser[Stmt] =
    located(rep1sep(variable, ",") ~ (":=" ~> exprs) <~ ";" ^^ { case xs ~ es =>
      Stmt.Assign(xs, es, _)
    })

  // an if after else ends the whole chain
  def ifs: Parser[Stmt] =
    located((kw("if") ~> expr) ~ (kw("then") ~> body) ~ otherwise ^^ { case c ~ l ~ r =>
      Stmt.If(c, l, r, _)
    })

  def otherwise: Parser[Stmt] =
    kw("else") ~> (ifs | body <~ kw("end")) | located(kw("end") ^^^ (Stmt.Skip(_)))

  def loop: Parser[Stmt] =
    located((kw("while") ~> expr) ~ (kw("do") ~> body <~ kw("end")) ^^ { case c ~ b =>
      Stmt.While(c, b, _)
    })

  def call: Parser[Stmt] =
    located((kw("call") ~> name) ~ parens(sections(list(expr), list(variable))) <~ ";" ^^ {
      case p ~ ((ins, outs, refs)) =>
        Stmt.Call(p, ins, outs, refs, _)
    })

  // the body of an if, while, or procedure, which may declare variables first
  def body: Parser[Stmt] =
    located(rep(locals) ~ rep(stmt) ^^ { case ls ~ ss =>
      Stmt.Block(ls.flatten, ss, _)
    })

  def locals: Parser[List[Pat.Var]] =
    kw("local") ~> rep1sep(formal, ",") <~ ";"

  def notation: Parser[Pos => Cmd] =
    val fixity =
      kw("prefix") ^^^ Fixity.Prefix
        | kw("postfix") ^^^ Fixity.Postfix
        | kw("infix") ~> (kw("left") ^^^ Fixity.Left | kw("right") ^^^ Fixity.Right | success(Fixity.Infix))
    fixity ~ ("""\d+""".r ^^ (_.toInt)) ~ rep1(symbol | name) ^^ { case f ~ p ~ ns =>
      Cmd.Notation(f, p, ns, _)
    }

  def sort: Parser[Pos => Cmd] =
    (kw("sort") ~> name) ~ typarams ~ ("=" ~> typ) ^^ { case n ~ ps ~ t =>
      Cmd.Sort(n, ps, t, _)
    }

  def data: Parser[Pos => Cmd] =
    val constr =
      located((name | symbol) ~ opt(parens(list(typ))) ^^ { case c ~ ts =>
        Constr(c, ts.getOrElse(Nil), _)
      })
    (kw("data") ~> name) ~ typarams ~ ("=" ~> rep1sep(constr, "|")) ^^ { case n ~ ps ~ cs =>
      Cmd.Data(n, ps, cs, _)
    }

  def fun: Parser[Pos => Cmd] =
    (kw("function") ~> (name | symbol)) ~ typarams ~ parens(formals) ~ (":" ~> typ) ~ ("=" ~> expr) ^^ {
      case n ~ ps ~ xs ~ r ~ b =>
        Cmd.Fun(n, ps, xs, r, b, _)
    }

  def proc: Parser[Pos => Cmd] =
    (kw("procedure") ~> name) ~ typarams ~ parens(sections(formals, formals)) ~ (body <~ kw("end")) ^^ {
      case n ~ ps ~ ((ins, outs, refs)) ~ b =>
        Cmd.Proc(n, ps, ins, outs, refs, b, _)
    }

  def cmd: Parser[Cmd] =
    located(
      kw("import") ~> name ^^ (n => Cmd.Import(n, _))
        | notation
        | sort
        | data
        | fun
        | proc
        | kw("eval") ~> expr ^^ (e => Cmd.Eval(e, _))
        | kw("run") ~> body <~ kw("end") ^^ (b => Cmd.Run(b, _))
        | kw("test") ~> body <~ kw("end") ^^ (b => Cmd.Test(b, _))
    )

  def parse(text: String): Program =
    parseAll(rep(cmd), text) match
      case Success(cs, _) =>
        Program(cs)
      case failure: NoSuccess =>
        throw new ulang.Error(s"parse error: ${failure.msg} at ${failure.next.pos}")
