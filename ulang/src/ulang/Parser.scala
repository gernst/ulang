package ulang

// params are the type parameters in scope, which distinguish Param from Sort, notations are the operators declared so far,
// modules are the modules loaded so far with their own notations, loading are the modules being parsed, which cannot be imported,
// base are the notations of the prelude, which modules are parsed with, or None while parsing the prelude,
// whose modules are parsed with its notations so far, and read gives the text of a module
case class Scope(
    params: Set[Id],
    notations: List[Cmd.Notation],
    modules: Map[Id, List[Cmd.Notation]] = Map(),
    loading: Set[Id] = Set(),
    base: Option[List[Cmd.Notation]] = None,
    read: Id => String = Scope.none
):
  def prefix(op: Id): Option[Int] =
    notations.collectFirst:
      case Cmd.Notation(Fixity.Prefix, prec, names) if names.contains(op) =>
        prec

  def postfix(op: Id): Option[Int] =
    notations.collectFirst:
      case Cmd.Notation(Fixity.Postfix, prec, names) if names.contains(op) =>
        prec

  def infix(op: Id): Option[(Fixity, Int)] =
    notations.collectFirst:
      case Cmd.Notation(fixity @ (Fixity.Infix | Fixity.Left | Fixity.Right), prec, names) if names.contains(op) =>
        (fixity, prec)

  def declared(op: Id): Boolean =
    notations.exists(_.names.contains(op))

object Scope:
  def none(name: Id): String =
    error("parse", s"unknown module $name")

// operands and operators of an expression or type, before their precedence is resolved,
// where operators remember their input for errors
enum Item[A]:
  case Operand(a: A)
  case Operator(name: Id, at: scala.util.parsing.input.Reader[Char])

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
    "skip",
    "test",
    "then",
    "true",
    "type",
    "try",
    "while",
    "with"
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
    s"$word\\b".r.withFailureMessage(s"$word expected")

  val name: Parser[Id] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^? (
      {
        case x if !keywords(x) =>
          x
      },
      x => s"keyword $x is not a name"
    )

  val symbol: Parser[Id] =
    "[-+*/%<>=!&|:^~?@#$]+".r ^? (
      {
        case x if !reserved(x) =>
          x
      },
      x => s"$x is not an operator"
    )

  // a name that is not declared as an operator
  def plain(s: Scope): Parser[Id] =
    name ^? (
      {
        case x if !s.declared(x) =>
          x
      },
      x => s"operator $x is not a name"
    )

  // a symbol, or a name declared as an operator
  def operator(s: Scope): Parser[Id] =
    symbol |
      name ^? {
        case x if s.declared(x) =>
          x
      }

  val names: Parser[List[Id]] = rep1sep(name, ",")

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
  )(prefix: (Id, A) => A, postfix: (Id, A) => A, infix: (Id, A, A) => A): Parser[A] =
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
  val product = Name("(product)")

  // a single type of a list, where () is the empty tuple, and only the arguments of a function are written (A, B)
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
      (op, t) => List(Type.Sort(Name(op), List(one(t)))),
      (op, t) => List(Type.Sort(Name(op), List(one(t)))),
      // the arguments of a function (A, B) -> C are a list, while A * B -> C takes a tuple
      (op, l, r) =>
        if op == "->" then
          List(Type.Fun(l.map(t => one(List(t))), one(r)))
        else if op == "*" then
          List(Type.Sort(product, components(l) ++ components(r)))
        else
          List(Type.Sort(Name(op), List(one(l), one(r))))
    ) ^? (
      {
        case ts if ts.length <= 1 =>
          one(ts)
      },
      _ => "tuple types are written A * B"
    )

  // a type, a type in parentheses, the arguments (A, B) of a function, (), or a procedure (in A, out B, ref C)
  def basic(s: Scope): Parser[List[Type]] =
    plain(s) ~ opt("[" ~> rep1sep(typ(s), ",") <~ "]") ^^ {
      case n ~ None if s.params(n) =>
        List(Type.Param(n))
      case n ~ args =>
        List(Type.Sort(Name(n), args.getOrElse(Nil)))
    } | parens(sections(typ(s), typ(s))) ^? { case (ins, outs, refs) if ins.nonEmpty || outs.nonEmpty || refs.nonEmpty =>
      List(Type.Proc(ins, outs, refs))
    } | parens(list(typ(s)))

  val typarams: Parser[List[Id]] = opt("[" ~> names <~ "]") ^^ (_.getOrElse(Nil))

  def variable(s: Scope): Parser[Expr.Var] =
    located(pos => plain(s) ^^ (x => Expr.Var(Name(x), Type.Hole)(pos)))

  // only parameters and locals have type annotations
  def untyped(s: Scope): Parser[Pat.Var] =
    located(pos => plain(s) ^^ (x => Pat.Var(Name(x), Type.Hole)(pos)))

  def formal(s: Scope): Parser[Pat.Var] =
    located(pos =>
      plain(s) ~ opt(":" ~> typ(s)) ^^ { case x ~ t =>
        Pat.Var(Name(x), t.getOrElse(Type.Hole))(pos)
      }
    )

  def formals(s: Scope): Parser[List[Pat.Var]] =
    list(formal(s))

  // raise e is a whole expression, not an operand, such that a statement raise after an expression starts a statement
  def expr(s: Scope): Parser[Expr] =
    located(pos => kw("raise") ~> expr(s) ^^ (Expr.Raise(_, Type.Hole)(pos))) | updates(s)

  // e with f := a, g := b updates the fields f and g of e by the updaters T.f:= and T.g:=, which the checker resolves by the type of e
  def updates(s: Scope): Parser[Expr] =
    operators(s) ~ opt(kw("with") ~> rep1sep(located(pos => name ~ (":=" ~> operators(s)) ^^ (pos -> _)), ",")) ^^ {
      case e ~ None =>
        e
      case e ~ Some(us) =>
        us.foldLeft(e):
          case (r, (pos, f ~ a)) =>
            Expr.App(Expr.Var(Name("." + f + ":="), Type.Hole)(pos), List(r, a))(pos)
    }

  def exprs(s: Scope): Parser[List[Expr]] =
    rep1sep(expr(s), ",")

  def operators(s: Scope): Parser[Expr] =
    resolved(s, items(s, app(s)))(prefix(_, _), op(_, _), op(_, _, _))

  // -n for a number n is the negative literal, in expressions and patterns, and otherwise a prefix operator is applied
  def prefix(f: Id, e: Expr): Expr =
    e match
      case Expr.Lit(n: BigInt) if f == "-" =>
        Expr.Lit(-n)(e.pos)
      case _ =>
        op(f, e)

  def prefix(f: Id, p: Pat): Pat =
    p match
      case Pat.Lit(n: BigInt) if f == "-" =>
        Pat.Lit(-n)(p.pos)
      case _ =>
        Pat.App(Name(f), List(p))(p.pos)

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
                  Expr.Match(e, List(Case(x, None, b)))(pos)
                case _ =>
                  Expr.Match(Expr.Tuple(es)(pos), List(Case(Pat.Tuple(xs)(pos), None, b)))(pos)
        }

  def ite(s: Scope): Parser[Expr] =
    located: pos =>
      (kw("if") ~> expr(s)) ~
        (kw("then") ~> expr(s)) ~
        (kw("else") ~> expr(s)) ^^ { case c ~ l ~ r =>
          Expr.Ite(c, l, r)(pos)
        }

  // lambda x, y => e is a function, and lambda in x, out y, ref z => body end a procedure, told apart by the modes
  def lam(s: Scope): Parser[Expr] =
    located: pos =>
      kw("lambda") ~> (
        (sections(formal(s), formal(s)) ^? { case (ins, outs, refs) if ins.nonEmpty || outs.nonEmpty || refs.nonEmpty =>
          (ins, outs, refs)
        }) ~ ("=>" ~> body(s) <~ kw("end")) ^^ { case (ins, outs, refs) ~ b =>
          Expr.Proc(ins, outs, refs, b)(pos)
        } |
          repsep(untyped(s), ",") ~ ("=>" ~> expr(s)) ^^ { case xs ~ b =>
            Expr.Lam(xs, b)(pos)
          }
      )

  // case p if g => b
  def cases[A](s: Scope, body: => Parser[A]): Parser[List[Case[A]]] =
    val guarded =
      kw("case") ~> pattern(s) ~ opt(kw("if") ~> expr(s)) ~ ("=>" ~> body) ^^ { case p ~ g ~ b =>
        Case(p, g, b)
      }
    rep(guarded) <~ kw("end")

  def matches[A, B](s: Scope, body: => Parser[A])(make: (Expr, List[Case[A]], Pos) => B): Parser[B] =
    located: pos =>
      (kw("match") ~> expr(s)) ~ cases(s, body) ^^ { case e ~ cs =>
        make(e, cs, pos)
      }

  // operators in patterns are constructors
  def pattern(s: Scope): Parser[Pat] =
    resolved(s, items(s, simple(s)))(
      prefix(_, _),
      (op, p) => Pat.App(Name(op), List(p))(p.pos),
      (op, l, r) => Pat.App(Name(op), List(l, r))(l.pos)
    )

  def simple(s: Scope): Parser[Pat] =
    located: pos =>
      plain(s) ~ parens(list(pattern(s))) ^^ { case c ~ ps =>
        Pat.App(Name(c), ps)(pos)
      } |
        value ^^
        (Pat.Lit(_)(pos)) | tuple(pattern(s))(Pat.Tuple(_)(pos)) | kw("_") ^^^ Pat.Wild()(pos) |
        brackets(pattern(s))(Pat.App(Name("Nil"), Nil)(pos), (p, ps) => Pat.App(Name("Cons"), List(p, ps))(pos)) |
        plain(s) ^^
        (x => Pat.Var(Name(x), Type.Hole)(pos))

  // [a, b] is a :: b :: Nil, and [a, b; c] is a :: b :: c, with at least one element before ;
  def brackets[A](p: => Parser[A])(nil: A, cons: (A, A) => A): Parser[A] =
    ("[" ~> list(p) ~ opt(";" ~> p) <~ "]") ^? (
      {
        case as ~ None =>
          as.foldRight(nil)(cons)
        case as ~ Some(rest) if as.nonEmpty =>
          as.foldRight(rest)(cons)
      },
      _ => "a list [a; b] needs elements before ;"
    )

  // applications of operators are at their first operand
  def op(f: Id, args: Expr*): Expr =
    Expr.App(Expr.Var(Name(f), Type.Hole)(args.head.pos), args.toList)(args.head.pos)

  // f(x) and e.f, which reads the field f of e, as the checker resolves it by the type of e
  def app(s: Scope): Parser[Expr] =
    atom(s) ~ rep(parens(list(expr(s))) ^^ (Left(_)) | "." ~> name ^^ (Right(_))) ^^ { case f ~ suffixes =>
      suffixes.foldLeft(f):
        case (e, Left(args)) =>
          Expr.App(e, args)(f.pos)
        case (e, Right(field)) =>
          Expr.App(Expr.Var(Name("." + field), Type.Hole)(f.pos), List(e))(f.pos)
    }

  // the characters escaped by \ in strings and characters
  val escapes = Map('"' -> '"', '\'' -> '\'', '\\' -> '\\', 'n' -> '\n', 't' -> '\t', 'r' -> '\r')

  // the characters between quotes with their escapes resolved, or None for an unknown escape
  def unescape(cs: List[Char]): Option[List[Char]] =
    cs match
      case Nil =>
        Some(Nil)
      case '\\' :: c :: rest if escapes.contains(c) =>
        unescape(rest) match
          case Some(ds) =>
            Some(escapes(c) :: ds)
          case None =>
            None
      case '\\' :: _ =>
        None
      case c :: rest =>
        unescape(rest) match
          case Some(ds) =>
            Some(c :: ds)
          case None =>
            None

  def quoted(s: String): Option[List[Char]] =
    unescape(s.toList.drop(1).dropRight(1))

  val value: Parser[Any] =
    kw("true") ^^^ true | kw("false") ^^^ false |
      """\d+""".r ^^
      (BigInt(_)) |
      "\"([^\"\\\\\n]|\\\\.)*\"".r ^^ quoted ^? (
        { case Some(cs) =>
          cs.mkString
        },
        _ => "invalid escape in a string"
      ) |
      "'([^'\\\\\n]|\\\\.)'".r ^^ quoted ^? (
        { case Some(List(c)) =>
          c
        },
        _ => "invalid character"
      )

  // let, if, and lambda extend as far to the right as possible, also as the last operand of an operator
  def atom(s: Scope): Parser[Expr] =
    located: pos =>
      let(s) | ite(s) | lam(s) | matches(s, expr(s))(Expr.Match(_, _)(_)) |
        value ^^
        (Expr.Lit(_)(pos)) | tuple(expr(s))(Expr.Tuple(_)(pos)) | comp(s) |
        brackets(expr(s))(Expr.Var(Name("Nil"), Type.Hole)(pos), op("Cons", _, _)) | braces(s) | variable(s)

  // {a, b} is the set from_list([a, b]) and {k => v, l => w} the map put_all([(k, v), (l, w)], empty()),
  // by the functions of the modules set and map, and {e for x in s if c} and {k => v for k1 => v1 in m if c} their comprehensions
  def braces(s: Scope): Parser[Expr] =
    located: pos =>
      val entry = expr(s) ~ opt("=>" ~> expr(s))
      ("{" ~> entry >> { first =>
        (kw("for") ~> comprehension(s, first, pos)) | (rep("," ~> entry) ^^ (first :: _) ^^ (literal(_, pos)))
      }) <~ "}"

  def library(m: Id, f: Id, pos: Pos, args: Expr*): Expr =
    Expr.App(Expr.Var(Name(Some(m), f), Type.Hole)(pos), args.toList)(pos)

  def literal(entries: List[Expr ~ Option[Expr]], pos: Pos): Expr =
    val nil: Expr = Expr.Var(Name("Nil"), Type.Hole)(pos)
    entries match
      case _ if entries.forall(_._2.isEmpty) =>
        val xs =
          entries.foldRight(nil):
            case (x ~ _, r) =>
              op("Cons", x, r)
        library("set", "from_list", pos, xs)
      case _ if entries.forall(_._2.nonEmpty) =>
        val ps =
          entries.foldRight(nil):
            case (k ~ v, r) =>
              op("Cons", Expr.Tuple(List(k, v.get))(pos), r)
        library("map", "put_all", pos, ps, library("map", "empty", pos))
      case _ =>
        error("parse", "a literal {...} has either elements a or entries k => v", pos)

  // sets range over sets, and maps over the entries of maps, each bound to a hidden variable whose key and value are matched
  def comprehension(s: Scope, first: Expr ~ Option[Expr], pos: Pos): Parser[Expr] =
    val test = opt(kw("if") ~> expr(s))
    first match
      case x ~ None =>
        rep1sep(untyped(s) ~ (kw("in") ~> expr(s)), ",") ~ test ^^ { case gs ~ t =>
          val lists =
            for case _ ~ l <- gs
            yield library("set", "elements", pos, l)
          library("set", "from_list", pos, Expr.Comp(x, gs.map(_._1), lists, t.getOrElse(Expr.Lit(true)(pos)))(pos))
        }
      case k ~ Some(v) =>
        rep1sep(untyped(s) ~ ("=>" ~> untyped(s)) ~ (kw("in") ~> expr(s)), ",") ~ test ^^ { case gs ~ t =>
          val entries =
            for case (a ~ b ~ _, i) <- gs.zipWithIndex
            yield
              val h: Pat.Var = Pat.Var(Name(s"(entry ${i + 1})"), Type.Hole)(pos)
              (h, a, b)
          val lists =
            for case (_ ~ _ ~ m, i) <- gs.zipWithIndex
            yield library("map", "entries", pos, unpack(entries.take(i), m, pos))
          val result = unpack(entries, Expr.Tuple(List(k, v))(pos), pos)
          val c = t.map(unpack(entries, _, pos)).getOrElse(Expr.Lit(true)(pos))
          library("map", "put_all", pos, Expr.Comp(result, entries.map(_._1), lists, c)(pos), library("map", "empty", pos))
        }

  // e where the key and value of each hidden variable are bound
  def unpack(entries: List[(Pat.Var, Pat.Var, Pat.Var)], e: Expr, pos: Pos): Expr =
    entries match
      case Nil =>
        e
      case (h, k, v) :: rest =>
        Expr.Match(Expr.Var(h.name, Type.Hole)(pos), List(Case(Pat.Tuple(List(k, v))(pos), None, unpack(rest, e, pos))))(pos)

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
        (kw("catch") ~> cases(s, body(s))) ^^ { case b ~ cs =>
          Stmt.Try(b, cs.map(c => Handler(Type.Hole, c.pat, c.guard, c.body)))(pos)
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
        Stmt.Call(Name(p), ins, outs, refs)(pos)
      }

  // the body of an if, while, case, or procedure, which may declare variables first
  def body(s: Scope): Parser[Stmt] =
    located: pos =>
      rep(locals(s)) ~ rep(stmt(s)) ^^ { case ls ~ ss =>
        val decls = ls.flatten
        val inits =
          for case (_, Some(a)) <- decls
          yield a
        Stmt.Block(decls.map(_._1), inits ++ ss)(pos)
      }

  def locals(s: Scope): Parser[List[(Pat.Var, Option[Stmt])]] =
    kw("local") ~> rep1sep(local(s), ",")

  // x: T, x := e, or x: T := e, where e initializes x by an assignment at the start of the block,
  // in order, such that its type can be inferred
  def local(s: Scope): Parser[(Pat.Var, Option[Stmt])] =
    located: pos =>
      formal(s) ~ opt(":=" ~> expr(s)) ^^ { case x ~ e =>
        (x, e.map(a => Stmt.Assign(List(Expr.Var(x.name, Type.Hole)(x.pos) -> a))(pos)))
      }

  // a declaration that starts with keyword, name, and type parameters
  def generic(keyword: String, name: Parser[Id])(rest: (Id, List[Id], Scope) => Parser[Cmd]): Parser[Cmd] =
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
    generic("type", name): (n, ps, t) =>
      ":=" ~> typ(t.copy(notations = s.notations)) ^^ (Cmd.Sort(Name(n), ps, _)(pos))

  // with the accessors T.f of its named fields f
  def data(s: Scope, pos: Pos): Parser[List[Cmd]] =
    (kw("data") ~> name) ~ typarams >> { case n ~ ps =>
      val u = Scope(ps.toSet, s.notations)
      // C or C(T, U), where the arguments may be named fields, as in C(name: T, rest: U)
      val field = opt(name <~ ":") ~ typ(u) ^^ { case f ~ t =>
        (f, t)
      }
      val constr =
        (name | symbol) ~ opt(parens(list(field))) ^^ { case c ~ fs =>
          (Constr(Name(c), fs.getOrElse(Nil).map(_._2)), fs.getOrElse(Nil).map(_._1))
        }
      // the first constructor may be preceded by | as well
      ":=" ~> opt("|") ~> rep1sep(constr, "|") ^^ { ks =>
        for (k, fs) <- ks do
          val names = fs.flatten
          for f <- names.diff(names.distinct).headOption do
            error("parse", s"repeated field $f of ${k.name}")
        Cmd.Data(Name(n), ps, ks.map(_._1))(pos) :: accessors(n, ps, ks, pos) ++ updaters(n, ps, ks, pos)
      }
    }

  // the fields f of the constructors with each constructor, the position of f, its number of arguments, and the type of f
  def fieldsOf(ks: List[(Constr, List[Option[Id]])]): List[(Id, Name, Int, Int, Type)] =
    for
      (k, names) <- ks
      case ((Some(f), a), i) <- names.zip(k.args).zipWithIndex
    yield (f, k.name, i, k.args.length, a)

  def missingField(t: Id, f: Id, pos: Pos): Case[Expr] =
    Case(Pat.Wild()(pos), None, Expr.Raise(Expr.App(Expr.Var(Name("MissingField"), Type.Hole)(pos), List(Expr.Lit(s"$t.$f")(pos)))(pos), Type.Hole)(pos))

  // the accessor T.f of each field f, which raises MissingField if f is not a field of all constructors
  def accessors(t: Id, ps: List[Id], ks: List[(Constr, List[Option[Id]])], pos: Pos): List[Cmd] =
    val fields = fieldsOf(ks)
    for f <- fields.map(_._1).distinct
    yield
      val es = fields.filter(_._1 == f)
      require("parse", es.map(_._5).distinct.length == 1, s"field $f of $t has different types")
      val cases =
        for (_, k, i, n, _) <- es
        yield
          val ps = List.tabulate(n)(j => if j == i then Pat.Var(Name("y"), Type.Hole)(pos) else Pat.Wild()(pos))
          Case(Pat.App(k, ps)(pos), None, Expr.Var(Name("y"), Type.Hole)(pos))
      val missing = if es.length == ks.length then Nil else List(missingField(t, f, pos))
      val x: Pat.Var = Pat.Var(Name("x"), Type.Sort(Name(t), ps.map(Type.Param(_))))(pos)
      Cmd.Fun(Name(s"$t.$f"), ps, List(List(x)), es.head._5, Expr.Match(Expr.Var(Name("x"), Type.Hole)(pos), cases ++ missing)(pos))(pos)

  // the updater T.f:= of each field f, where x with f := y is C(a0, y, a2) for x = C(a0, _, a2)
  def updaters(t: Id, ps: List[Id], ks: List[(Constr, List[Option[Id]])], pos: Pos): List[Cmd] =
    val fields = fieldsOf(ks)
    for f <- fields.map(_._1).distinct
    yield
      val es = fields.filter(_._1 == f)
      val cases =
        for (_, k, i, n, _) <- es
        yield
          val ps = List.tabulate(n)(j => if j == i then Pat.Wild()(pos) else Pat.Var(Name(s"a$j"), Type.Hole)(pos))
          val args = List.tabulate(n)(j => Expr.Var(Name(if j == i then "y" else s"a$j"), Type.Hole)(pos))
          Case(Pat.App(k, ps)(pos), None, Expr.App(Expr.Var(k, Type.Hole)(pos), args)(pos))
      val missing = if es.length == ks.length then Nil else List(missingField(t, f, pos))
      val owner = Type.Sort(Name(t), ps.map(Type.Param(_)))
      val x: Pat.Var = Pat.Var(Name("x"), owner)(pos)
      val y: Pat.Var = Pat.Var(Name("y"), es.head._5)(pos)
      Cmd.Fun(Name(s"$t.$f:="), ps, List(List(x, y)), owner, Expr.Match(Expr.Var(Name("x"), Type.Hole)(pos), cases ++ missing)(pos))(pos)

  def fun(s: Scope, pos: Pos): Parser[Cmd] =
    generic("function", name | symbol): (n, ps, t) =>

      val u = t.copy(notations = s.notations)
      rep1(parens(formals(u))) ~
        (":" ~> typ(u)) ~
        (":=" ~> expr(u)) ^^ { case xss ~ r ~ b =>
          Cmd.Fun(Name(n), ps, xss, r, b)(pos)
        }

  def proc(s: Scope, pos: Pos): Parser[Cmd] =
    generic("procedure", name): (n, ps, t) =>

      val u = t.copy(notations = s.notations)
      parens(sections(formal(u), formal(u))) ~
        (":=" ~> body(u)) ^^ { case (ins, outs, refs) ~ b =>
          Cmd.Proc(Name(n), ps, ins, outs, refs, b)(pos)
        }

  def command(s: Scope): Parser[List[Cmd]] =
    located(pos => kw("import") ~> name ^^ (load(s, _, pos))) | located(pos => data(s, pos)) | declaration(s) ^^ (List(_))

  // the marker Import(n), preceded by module n at its first import, which is parsed with the notations of the prelude
  // and of its own imports, since imports are not transitive
  def load(s: Scope, n: Id, pos: Pos): List[Cmd] =
    if s.modules.contains(n) then
      List(Cmd.Import(n)(pos))
    else
      require("parse", !s.loading(n), s"cyclic import of $n")
      val text = s.read(n)
      val inner = Scope(Set(), s.base.getOrElse(s.notations), s.modules, s.loading + n, s.base, s.read)
      val cmds =
        try
          parse(commands(inner), text)
        catch
          case e: ulang.Error =>
            throw new ulang.Error(s"${e.getMessage} in $n")
      List(Cmd.Module(n, cmds.filter(declares))(pos), Cmd.Import(n)(pos))

  // modules contribute their declarations, but not their tests
  def declares(c: Cmd): Boolean =
    c match
      case Cmd.Test(_, _) =>
        false
      case _ =>
        true

  // the bodies of commands end at the keyword of the next command, so commands need no end
  def declaration(s: Scope): Parser[Cmd] =
    located: pos =>
      notation(s, pos) | sort(s, pos) | fun(s, pos) | proc(s, pos) |
        (kw("test") ~> name) ~ (":=" ~> body(s)) ^^ { case n ~ b =>
          Cmd.Test(n, b)(pos)
        }

  // notations apply to the commands after them
  def commands(s: Scope): Parser[List[Cmd]] =
    command(s) >> (cs => commands(cs.foldLeft(s)(declare)) ^^ (cs ++ _)) | success(Nil)

  // an import brings the own notations of the module
  def declare(s: Scope, c: Cmd): Scope =
    c match
      case n: Cmd.Notation =>
        s.copy(notations = s.notations :+ n)
      case d: Cmd.Module =>
        s.copy(modules = s.modules ++ modules(d))
      case Cmd.Import(n) =>
        s.copy(notations = s.notations ++ s.modules.getOrElse(n, Nil))
      case _ =>
        s

  // the scope after cmds
  def after(cmds: List[Cmd], s: Scope): Scope =
    cmds.foldLeft(s)(declare)

  // module d and the modules it loads, with their own notations
  def modules(d: Cmd.Module): Map[Id, List[Cmd.Notation]] =
    val nested =
      for case m: Cmd.Module <- d.cmds
      yield modules(m)
    nested.foldLeft(Map(d.name -> notations(d.cmds)))(_ ++ _)

  def notations(cmds: List[Cmd]): List[Cmd.Notation] =
    cmds.collect:
      case n: Cmd.Notation =>
        n

  def parse[A](p: Parser[A], text: String): A =
    parseAll(p, text) match
      case Success(a, _) =>
        a
      case failure: NoSuccess =>
        error("parse", failure.msg, failure.next.pos)

  // the program text in scope s, e.g., after the prelude
  def parseProgram(text: String, s: Scope): Program =
    Program(parse(commands(s), text))

// precedence climbing, where prefix operators bind their operand with their own precedence
case class Resolve[A](s: Scope, prefix: (Id, A) => A, postfix: (Id, A) => A, infix: (Id, A, A) => A):
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

  def nonassociative(op: Id, fixity: Fixity, prec: Int, rest: List[Item[A]]): Unit =
    rest match
      case Item.Operator(next, at) :: _ if fixity == Fixity.Infix && s.infix(next).exists(_._2 == prec) =>
        fail(s"operator $op is not associative", Some(at))
      case _ =>
