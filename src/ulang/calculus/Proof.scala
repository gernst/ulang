package ulang.calculus

import ulang.syntax._

trait Proof {
  def format(ident: Int): String
  def isClosed: Boolean
}

case class Step(prems: List[Proof], concl: Seq, rule: Rule) extends Proof {
  def format(ident: Int): String = {
    val sp = "  " * ident
    var res = ""
    res += concl.format(ident) + " by " + rule + "\n"
    for (prem <- prems) {
      res += prem.format(ident + 1)
    }
    res
  }

  override def toString = format(0)
  def isClosed = prems forall (_.isClosed)
}

case class Seq(ant: List[Expr], suc: Expr) extends Proof {
  def format(ident: Int): String = ("  " * ident) + this
  override def toString = ant.mkString(", ") + " ⊦ " + suc
  def isClosed = false
}
