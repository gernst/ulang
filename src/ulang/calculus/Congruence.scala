package ulang.calculus

import ulang.syntax._
import de.gidonernst.util.DisjointSets

case class Congruence(var _cong: DisjointSets[Expr], var _use: Map[Expr, Set[Expr]], var _sig: Map[Expr, Expr]) {
  def find(e: Expr) = _cong find e
  def union(e1: Expr, e2: Expr) { _cong = _cong union (e1, e2) }
  def use(e: Expr) = _use.getOrElse(e, Set.empty)
  def sig(e: Expr) = _sig.getOrElse(e, e)
  
  def +(e: Expr) = e match {
    case ulang.syntax.predefined.pred.Eq(e1, e2) =>
      val cc = copy()
      cc.merge(e1, e2)
      cc
  }

  def merge(e1: Expr, e2: Expr) {
    if (e1 != e2) {
      union(e1, e2)
      for(u <- use(e1)) {
        _sig += u -> (sig(u) replace (e1, e2))
        for(v <- use(e2) if sig(v) == sig(u)) {
          merge(find(u), find(v))
        }
        _use += e2 -> (use(e2) + u)
      }
    }
  }

  def canon(e: Expr): Expr = e match {
    case App(fun, arg) =>
      canonsig(App(canon(fun), canon(arg)))
    case _ =>
      canonsig(e)
  }

  def canonsig(e: Expr): Expr = e match {
    case App(fun, arg) =>
      use(arg) find (_ == e) match {
        case Some(u) =>
          find(u)
        case None =>
          _use += arg -> (use(arg) + e)
          _use = _use - e
          e
      }
    case _ =>
      _cong find e
  }
}

object Congruence {
  def empty = Congruence(DisjointSets.empty, Map.empty, Map.empty)
}