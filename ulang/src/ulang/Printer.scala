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
      case Obj("nil" | "cons", _) if items(v).nonEmpty =>
        items(v).get.map(show).mkString("[", ", ", "]")
      case Obj(c, Nil) =>
        c
      case Obj(c, vs) =>
        vs.map(show).mkString(s"$c(", ", ", ")")
      case _ =>
        v.toString

  // the elements of a list built by nil and cons
  def items(v: Any): Option[List[Any]] =
    v match
      case Obj("nil", Nil) =>
        Some(Nil)
      case Obj("cons", List(x, xs)) =>
        items(xs).map(x :: _)
      case _ =>
        None
