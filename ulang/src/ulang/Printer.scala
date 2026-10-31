package ulang

object Printer:
  def show(v: Any): String =
    v match
      case () =>
        "()"
      case s: String =>
        "\"" + s + "\""
      case _: Function1[?, ?] =>
        "<function>"
      case Obj(",", vs) =>
        vs.map(show).mkString("(", ", ", ")")
      case Obj("::", _) if items(v).nonEmpty =>
        items(v).get.map(show).mkString("[", ", ", "]")
      case Obj(c, Nil) =>
        c
      case Obj(c, vs) =>
        vs.map(show).mkString(s"$c(", ", ", ")")
      case _ =>
        v.toString

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
      case Pat.Wild() =>
        "_"
      case Pat.Lit(value) =>
        show(value)
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

  // the elements of a list built by nil and ::
  def items(v: Any): Option[List[Any]] =
    v match
      case Obj("nil", Nil) =>
        Some(Nil)
      case Obj("::", List(x, xs)) =>
        items(xs).map(x :: _)
      case _ =>
        None
