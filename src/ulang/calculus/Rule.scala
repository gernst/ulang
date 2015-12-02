package ulang.calculus

import arse.control._

trait Rule extends (Seq => Proof) {
  def name: String
  override def toString = name
}

case class SimpleRule(name: String, f: PartialFunction[Seq, List[Seq]]) extends Rule {
  def apply(seq: Seq): Proof = {
    if (f.isDefinedAt(seq)) Step(f(seq), seq, this)
    else fail
  }
}
