package ulang.typ

class Unary(val name: String) extends (Type => Type) {
  def unapply(e: Type) = e match {
    case App(`name`, arg) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Type) = {
    App(name, List(arg))
  }
}

class Binary(val name: String) extends ((Type, Type) => Type) {
  def unapply(e: Type) = e match {
    case App(`name`, List(arg1, arg2)) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Type, arg2: Type) = {
    App(name, List(arg1, arg2))
  }
}