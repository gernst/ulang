package ulang.calculus

import ulang.syntax._
import ulang.expr.Op
import ulang.expr.FreeVar
import ulang.expr.Expr
import ulang.expr.Bound
import ulang.typ.App

case class Structural(thy: Thy, x: FreeVar) extends Rule {
  def name = "structural induction on " + x

  // note: assume that x is also properly typed wrt the goal

  val cases: List[Expr] = {
    ???
    /*
    val ta @ App(con, args) = x.typ
    val constrs: List[Op] = thy.df.data(ta.con).toList
    constrs map {
      case op @ Op(_, App(con, args)) =>
        val free = args map { FreeVar("x", _) }
        val bound = (1 to args.length) map { BoundVar(_) }
        // Eq(x, Ex(free, Apps(op, bound toList)))
        ???
    }*/
  }

  def apply(seq: Seq) = {
    Step(cases map (_ :: seq), seq, this)
  }
}