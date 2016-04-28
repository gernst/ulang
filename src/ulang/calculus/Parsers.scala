package ulang.calculus

import arse._
import arse._
import arse.Combinators._
import ulang.Context
import ulang.syntax._

class Parsers(thy: Thy) extends ulang.syntax.Parsers(thy) {
  val cut = lit("cut") ~> parse(Cut)(formula)
  
  val simplify = lit("simplify") ~> ret(Simplify)
  
  val structural = lit("induction") ~> expr map {
    case x: FreeVar =>
      Structural(thy, x)
  }

  def rule = simplify | cut | structural
}