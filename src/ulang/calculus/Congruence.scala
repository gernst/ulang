package ulang.calculus

import ulang.syntax._
import ulang.DisjointSets

class Congruence(var _cong: DisjointSets[Expr], var _use: Map[Expr, Set[Expr]], var _sig: Map[Expr, Expr]) {
  def find(e: Expr) = _cong find e
  def union(e1: Expr, e2: Expr) { _cong = _cong union (e1, e2) }
  def use(e: Expr) = _use.getOrElse(e, Set.empty)
  def sig(e: Expr) = _sig(e)

  def merge(e1: Expr, e2: Expr) = {
    val cc = new Congruence(_cong, _use, _sig)
    cc _process (e1, e2)
    cc
  }

  def canon(e: Expr): Expr = e match {
    case App(fun, arg) =>
      canonsig(App(canon(fun), canon(arg)))
    case _ =>
      canonsig(e)
  }
  
  private def _process(e1: Expr, e2: Expr) {
    _merge(canon(e1), canon(e2))
  }

  private def _merge(e1: Expr, e2: Expr) {
    if (e1 != e2) {
      union(e1, e2)
      for (u <- use(e1)) {
        _sig += u -> (sig(u) replace (e1, e2))
        for (v <- use(e2) if sig(v) == sig(u)) {
          _merge(find(u), find(v))
        }
        _use += e2 -> (use(e2) + u)
      }
    }
  }

  private def canonsig(e: Expr): Expr = e match {
    case App(fun, arg) =>
      use(arg) find (sig(_) == e) match {
        case Some(u) =>
          find(u)
        case None =>
          _use += arg -> (use(arg) + e)
          _sig += e -> e
          _use = _use - e
          e
      }
    case _ =>
      find(e)
  }

  override def toString = _cong.toString + " | use: " + _use + " | sig: " + _sig  
}

object Congruence {
  def empty = new Congruence(DisjointSets.empty, Map.empty, Map.empty)
}