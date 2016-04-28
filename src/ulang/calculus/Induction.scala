package ulang.calculus

import ulang.syntax._

case class Structural(thy: Thy, x: FreeVar) extends Rule {
  def name = "structural induction on " + x
  import ulang.syntax.predefined.pred._

  // note: assume that x is also properly typed wrt the goal

  val cases: List[Expr] = {
    val TypeApp(con, args) = x.typ
    val constrs: List[Op] = thy.df.data(Con(con, args.length)).toList
    constrs map {
      case op @ Op(_, TypeApp(con, args)) =>
        val free = args map { FreeVar("x", _) }
        val bound = (1 to args.length) map { BoundVar(_) }
        Eq(x, Ex(free, App(op, bound toList)))
    }
  }

  def apply(seq: Seq) = {
    Step(cases map (_ :: seq), seq, this)
  }
}