package ulang.calculus

import arse._
import ulang._
import ulang.syntax._

class Parsers(thy: Thy) extends ulang.syntax.Parsers(thy) {
  import Parser._
  import Recognizer._
  
  val cut = "cut" ~ Cut.from(formula)
  
  val simplify = "simplify" ~ ret[String, Rule](Simplify)
  
  val structural = "induction" ~ expr map {
    case x: FreeVar =>
      Structural(thy, x)
  }

  def rule = simplify | cut | structural
}