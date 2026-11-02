package ulang

object Printer:
  def show(v: Any): String =
    v match
      case s: String =>
        "\"" + s.toList.map(escape(_, '"')).mkString + "\""
      case c: Char =>
        "'" + escape(c, '\'') + "'"
      case _: Function1[?, ?] =>
        "<function>"
      case _: Proc =>
        "<procedure>"
      case Obj(Names.tuple, vs) =>
        vs.map(show).mkString("(", ", ", ")")
      case Obj(Names.cons, _) if items(v).nonEmpty =>
        items(v).get.map(show).mkString("[", ", ", "]")
      case Obj(c, Nil) =>
        c.toString
      case Obj(c, vs) =>
        vs.map(show).mkString(s"$c(", ", ", ")")
      case _ =>
        v.toString

  // constructors named by operators are shown infix
  def symbolic(name: Id): Boolean =
    !name.head.isLetter && name.head != '_' && name != ","

  def infix(op: Id, l: String, r: String): String =
    def operand(a: String): String =
      if a.contains(" ") && !a.startsWith("(") then
        s"($a)"
      else
        a
    s"${operand(l)} $op ${operand(r)}"

  def show(p: Pat): String =
    p match
      case Pat.Wild() =>
        "_"
      case Pat.Lit(value) =>
        show(value)
      case Pat.Var(name, _) =>
        name.toString
      case Pat.Tuple(items) =>
        items.map(show(_)).mkString("(", ", ", ")")
      // before checking, the list constructor is not resolved yet
      case Pat.App(name, List(x, xs)) if name == Names.cons || name == Name("Cons") =>
        s"[${show(x)}; ${show(xs)}]"
      case Pat.App(name, List(l, r)) if symbolic(name.base) =>
        infix(name.base, show(l), show(r))
      case Pat.App(name, Nil) =>
        name.toString
      case Pat.App(name, args) =>
        args.map(show(_)).mkString(s"$name(", ", ", ")")

  // c in a string or character delimited by quote, as the parser reads it
  def escape(c: Char, quote: Char): String =
    c match
      case '\\' =>
        "\\\\"
      case '\n' =>
        "\\n"
      case '\t' =>
        "\\t"
      case '\r' =>
        "\\r"
      case `quote` =>
        s"\\$quote"
      case _ =>
        c.toString

  // the elements of a list built by Nil and Cons
  def items(v: Any): Option[List[Any]] =
    v match
      case Obj(Names.nil, Nil) =>
        Some(Nil)
      case Obj(Names.cons, List(x, xs)) =>
        items(xs).map(x :: _)
      case _ =>
        None
