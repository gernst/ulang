package ulang.calculus

import ulang.syntax._
import ulang.syntax.predefined.prop._
import ulang.syntax.predefined.pred._
import scala.annotation.tailrec

object Simplify extends Rule {
  def name = "simplify"

  type Rewrites = Map[Op, (List[Expr], List[Expr]) => Expr]

  def simp(phi: Expr, ant: List[Expr]): Expr = phi match {
    case True | False => phi
    case Not(phi) => not(phi, ant)
    case And(phi, psi) => and(phi, psi, ant)
    case Or(phi, psi) => or(phi, psi, ant)
    case Imp(phi, psi) => imp(phi, psi, ant)
    case Eqv(phi, psi) => eqv(phi, psi, ant)
    case _ => literal(phi, ant)
  }

  def apply(seq: Seq): Proof = {
    val ant = con(seq.ant, Nil)

    // contradicting assumptions
    if (ant contains False) {
      Step(Nil, seq, this)
    } else {
      val suc = simp(seq.suc, ant)

      if (suc == True)
        Step(Nil, seq, this)
      else
        Step(List(Seq(ant, suc)), seq, this)
    }
  }

  def rewrite(self: Expr, ant: List[Expr], rw: Rewrites): Expr = {
    rewrite(self, Nil, ant, rw)
  }

  @tailrec
  def rewrite(self: Expr, args: List[Expr], ant: List[Expr], rw: Rewrites): Expr = self match {
    case op: Op =>
      rw(op)(args, ant)
    case App(fun, arg) =>
      rewrite(fun, simp(arg, ant) :: args, ant, rw)
    case _ =>
      self
  }

  @tailrec
  def con(todo: List[Expr], ant: List[Expr]): List[Expr] = todo match {
    case Nil => ant.reverse
    case phi :: rest =>
      // may produce duplicate work:
      val newant = assume(rest, ant)
      val newphi = simp(phi, newant)
      con(rest, assume(newphi, ant))
  }
  
  def imps(suc: Expr): (List[Expr], Expr) = suc match {
    case Imp(phi, psi) =>
      val (ant, newsuc) = imps(psi)
      (phi :: ant, newsuc)
    case _ =>
      (Nil, suc)
  }
  
  def literal(phi: Expr, ant: List[Expr]): Expr = {
    if (ant contains phi) True
    else if (ant contains Not(phi)) False
    else phi
  }

  def not(phi: Expr, ant: List[Expr]): Expr = {
    triv.not(simp(phi, ant))
  }

  def and(phi: Expr, psi: Expr, ant: List[Expr]): Expr = {
    val (newphi, newpsi) = binary(phi, true, psi, true, ant)
    triv.and(newphi, newpsi)
  }

  def or(phi: Expr, psi: Expr, ant: List[Expr]): Expr = {
    val (newphi, newpsi) = binary(phi, false, psi, false, ant)
    triv.or(newphi, newpsi)
  }

  def imp(phi: Expr, psi: Expr, ant: List[Expr]): Expr = {
    val (newphi, newpsi) = binary(phi, true, psi, false, ant)
    triv.imp(newphi, newpsi)
  }

  def eqv(phi: Expr, psi: Expr, ant: List[Expr]): Expr = {
    val lr = simp(Imp(phi, psi), ant)
    val rl = simp(Imp(psi, phi), ant)

    // try to extract con/dis

    (lr, rl) match {
      case (False, _) => False
      case (_, False) => False
      case (True, _) => rl // already simplified
      case (_, True) => lr
      case _ =>
        triv.eqv(simp(phi, ant), simp(psi, ant)) // does a lot of work again
    }
  }

  def binary(
    phi: Expr, phi_pos: Boolean,
    psi: Expr, psi_pos: Boolean,
    ant: List[Expr],
    psi_done: Boolean = false,
    swap: Boolean = false): (Expr, Expr) =
    {
      val newant = if (psi_pos) assume(psi, ant) else assert(psi, ant)
      val newphi = simp(phi, newant)
      val phi_done = phi == newphi

      if (phi_done && psi_done) {
        if (swap) (psi, phi)
        else (phi, psi)
      } else {
        binary(psi, psi_pos, newphi, phi_pos, ant, phi_done, !swap)
      }
    }

  def push(phi: Expr, ant: List[Expr]): List[Expr] = {
    if (phi == False) List(phi)
    else if (phi == True) ant
    else phi :: ant
  }

  def assume(phi: Expr, ant: List[Expr]): List[Expr] = phi match {
    case Not(psi) => assert(psi, ant)
    case And(phi, psi) => assume(phi, assume(psi, ant))
    /*case Ex(bound, body) => // slow
      val avoid = free(phi :: ant)
      assume(inst(body, fresh(bound, avoid)), ant)*/
    case _ => push(phi, ant)
  }

  def assert(phi: Expr, ant: List[Expr]): List[Expr] = phi match {
    case Not(psi) => assume(psi, ant)
    case Imp(phi, psi) => assert(psi, assume(phi, ant))
    case Or(phi, psi) => assert(psi, assert(phi, ant))
    /*case All(bound, body) => // slow
      val avoid = free(phi :: ant)
      assert(inst(body, fresh(bound, avoid)), ant)*/
    case _ => push(triv.not(phi), ant)
  }

  def assume(args: List[Expr], ant: List[Expr]): List[Expr] = {
    args.foldRight(ant)(assume)
  }

  def assert(args: List[Expr], ant: List[Expr]): List[Expr] = {
    args.foldRight(ant)(assert)
  }
}