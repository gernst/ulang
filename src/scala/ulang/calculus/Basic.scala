package ulang.calculus

import arse._
import ulang.syntax._
import ulang.calculus._
import ulang.expr.Expr

case class Cut(phi: Expr) extends Rule {
  val name = "cut " + phi

  def apply(seq: Seq) = seq match {
    case Seq(phis) =>
      val prem1 = Seq(triv.not(phi) :: phis) // use a Seq.shift/rotate operation here!
      val prem2 = Seq(phi :: phis)
      Step(List(prem1, prem2), seq, this)
  }
}
