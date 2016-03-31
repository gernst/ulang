package ulang.source

sealed trait Expr {
}

case class Id(name: String) extends Expr with Type {
  override def toString = name
}

case class App(fun: Expr, arg: Expr) extends Expr {
  override def toString = "(" + fun + " " + arg + ")"
}

case class Lambda(bound: Id, body: Expr) extends Expr {
  override def toString = "(λ " + bound + ". " + body + ")"
}

object App {
  def apply(fun: Expr, args: List[Expr]): Expr = {
    args.foldLeft(fun)(App(_, _))
  }
}

object Lambda {
  def apply(bound: List[Id], body: Expr): Expr = {
    bound.foldRight(body)(Lambda(_, _))
  }
}

sealed trait Type {
  def →(that: Type): Type = TypeApp("→", List(this, that))
  
  def generic: Type = ??? /* {
    var ren: Map[String, TypeVar] = Map.empty

    def gen_rec(typ: Type): Type = typ match {
      case TypeParam(name) if ren contains name =>
        ren(name)
      case TypeParam(name) =>
        val alpha = TypeVar.fresh
        ren += name -> alpha
        alpha
      case TypeApp(name, args) =>
        new TypeApp(name, args map gen_rec)
      case _ =>
        fatal("in gen: " + typ + " unexpected")
    }

    gen_rec(this)
  } */
}

case class TypeVar(index: Int) extends Type {
  override def toString = "?" + index
}

case class TypeApp(name: String, args: List[Type] = Nil) extends Type {
  override def toString = (name :: args).mkString("(", " ", ")")
}

object TypeVar {
  var index = 0
  def nextIndex = { index += 1; index }

  def fresh: TypeVar = TypeVar(nextIndex)
}
