package ulang.calculus

import ulang.Context
import ulang.syntax._

case class Structural(ctx: Context, x: FreeVar) extends Rule {
  def name = "structural induction on " + x
  import ulang.syntax.predefined.pred._

  // note: assume that x is also properly typed wrt the goal

  val cases: List[Expr] = {
    val TypeApp(con, args) = x.typ
    val constrs = ctx.df.data(Con(con, args.length)).toList
    constrs map {
      case op @ Op(_, TypeApp(con, args)) =>
        val free = args map { FreeVar("x", _)}
        val bound = (1 to args.length) map { BoundVar(_) }
        Eq(x, Ex(free, App(op, bound toList)))
    }
  }

  def apply(seq: Seq) = seq match {
    case Seq(ant, suc) =>
      Step(cases map { (phi: Expr) => Seq(phi :: ant, suc) }, seq, this)
  }
}