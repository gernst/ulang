package ulang.calculus

import arse.control._
import ulang.syntax._
import ulang.syntax.predefined._
import ulang.calculus._

object Basic {
  object True extends SimpleRule("trivial", {
    case Seq(_, prop.True) => Nil
  })

  /*
object FalseLeft extends SimpleRule("false left", {
  case Seq(ant, _) if ant contains False => Nil
})
*/

  object Assumption extends SimpleRule("assumption", {
    case Seq(ant, suc) if ant contains suc => Nil
  })

  object Refl extends SimpleRule("reflexivity", {
    case Seq(_, pred.Eq(e1, e2)) if e1 == e2 => Nil
  })

  object Imp extends SimpleRule("implication", {
    case Seq(ant, prop.Imp(phi, psi)) => List(Seq(phi :: ant, psi))
  })

  object Not extends SimpleRule("negation", {
    case Seq(ant, prop.Not(prop.Not(phi))) => List(Seq(ant, phi))
  })

  object Or1 extends SimpleRule("disjunction 1", {
    case Seq(ant, prop.Or(phi, psi)) => List(Seq(triv.not(phi) :: ant, psi))
  })

  object Or2 extends SimpleRule("disjunction 2", {
    case Seq(ant, prop.Or(phi, psi)) => List(Seq(triv.not(psi) :: ant, phi))
  })

  object And extends SimpleRule("conjunction", {
    case Seq(ant, prop.And(phi, psi)) => List(Seq(ant, phi), Seq(ant, psi))
  })

  object Eqv extends SimpleRule("equivalence", {
    case Seq(ant, prop.Eqv(phi, psi)) => List(Seq(phi :: ant, psi), Seq(psi :: ant, phi))
  })

  case class Cut(phi: Expr) extends Rule {
    val name = "cut " + phi

    def apply(seq: Seq) = seq match {
      case Seq(ant, suc) =>
        val prem1 = Seq(triv.not(suc) :: ant, phi) // use a Seq.shift/rotate operation here!
        val prem2 = Seq(phi :: ant, suc)
        Step(List(prem1, prem2), seq, this)
    }
  }

  case class Rotate(n: Int) extends Rule {
    val name = "rotate " + n

    def apply(seq: Seq) = seq match {
      case Seq(ant, suc) if n < ant.length =>
        val new_ant = ant.take(n - 1) ++ ant.drop(n + 1)
        val new_suc = ant(n)
        Step(List(Seq(triv.not(suc) :: new_ant, triv.not(new_suc))), seq, this)
      case _ =>
        fail
    }
  }

  val rules = List(True, Assumption, Refl, Not, Imp, And, Or1, Or2, Eqv)
}