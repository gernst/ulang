package ulang.calculus

import ulang.syntax._
import ulang.syntax.predefined.prop._

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

case class Seq(phis: List[Expr]) extends Proof {
  def format(ident: Int): String = ("  " * ident) + this

  def ant = phis filter {
    case Not(_) => false
    case _ => true
  }

  def suc = phis collect {
    case Not(phi) => phi
  }

  override def toString = {
    ant.mkString(", ") + " ⊦ " + suc.mkString(", ")
  }

  def ::(phi: Expr) = Seq(phi :: phis)
  def isClosed = false
}

object Seq {
  def apply(ant: List[Expr], suc: List[Expr]): Seq = {
    Seq(ant ::: (suc map Not))
  }
}