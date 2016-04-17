package ulang.syntax

object Printer {
  def print(expr: Expr, prec: Int) = expr match {
    case FlatApp(fun, args) => fun match {
      case Op(name, _) =>
        ???
      case FreeVar(name, _) =>
        name
    }
  }
}