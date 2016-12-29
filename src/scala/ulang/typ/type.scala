package ulang.typ

import arse._

case class Con(name: String, arity: Int) {
  override def toString = name + "/" + arity
}

object Con {
  val function = Con("→", 2)
  val bool = Con("Bool", 0)
}

sealed trait Poly[+C, +A] {
  override def toString = this match {
    case Forall(as, body) => body format as
  }
}

sealed trait Mono[+C, +A] {
  override def toString = format(Nil)
  
  def format[A1 >: A](s: List[A1]): String = this match {
    case Bound(n) => s(n).toString
    case Free(a) => a.toString
    case App(c, args) => c + args.map(_ format s).mkString("(", " ", ")")
  }
  
  def bind[A1 >: A, C1 >: C](ts: List[Mono[C1, A1]]): Mono[C1, A1] = this match {
    case Bound(_) => this
    case t if ts contains t => Bound(ts indexOf t)
    case Free(b) => Free(b)
    case App(c, args) => App(c, args map (_ bind ts))
  }

  def unbind[A1 >: A, C1 >: C](ts: List[Mono[C1, A1]]): Mono[C1, A1] = this match {
    case Bound(n) => ts(n)
    case Free(a) => Free(a)
    case App(c, args) => App(c, args map (_ unbind ts))
  }

  def rename[B](r: A => B): Mono[C, B] = this match {
    case Bound(n) => Bound(n)
    case Free(a) => Free(r(a))
    case App(c, args) => App(c, args map (_ rename r))
  }

  def subst[A1 >: A, C1 >: C](s: Subst[C1,A1]): Mono[C1, A1] = this match {
    case Bound(n) => this
    case Free(a) => s(a)
    case App(c, args) => App(c, args map (_ subst s))
  }

  def eval(s: List[Carrier], e: A => Carrier, m: C => Carrier): Carrier = this match {
    case Bound(n) => s(n)
    case Free(a) => e(a)
    case App(c, args) =>
      m(c) match {
        case r: Relation @unchecked =>
          val as = args map (_ eval (s, e, m));
          {
            case bs: List[Any] =>
              (as, bs).zipped forall { case (a, b) => a(b) }
          }
      }
  }

  def reduce[C1 >: C, A1 >: A](m: C1 => Option[Poly[C1, A1]]): Mono[C1, A1] = this match {
    case Bound(n) => Bound(n)
    case Free(_) => this
    case App(c, args) =>
      val as = args map (_ reduce m)
      m(c) match {
        case Some(Forall(bs, body)) =>
          body bind as
        case None =>
          App(c, as)
      }
  }

  def unify[C1 >: C, A1 >: A](that: Mono[C1, A1]): Option[Subst[C1,A1]] = (this, that) match {
    case _ if this == that =>
      Some(Subst.empty)
    case (f @ Free(a), _) if !(f in that) =>
      Some(Subst.singleton(a, that))
    case (_, a: Free[_]) =>
      that unify this
    case (App(c1, args1), App(c2, args2)) if c1 == c2 && args1.length == args2.length =>
      (args1, args2).zipped.foldLeft(Option(Subst.empty[C1, A1])) {
        case (Some(s), (t1, t2)) =>
          (t1 subst s) unify (t2 subst s) map (s o _)
        case (None, _) =>
          None
      }
    case _ =>
      None
  }
}

case class Bound(n: Int) extends Mono[Nothing, Nothing]

case class Free[+A](a: A) extends Mono[Nothing, A] {
  def in[B >: A](t: Mono[B, _]): Boolean = t match {
    case Bound(_) => false
    case Free(_) => t == this
    case App(c, args) => args exists (this in _)
  }
}

case class App[+C, +A](c: C, args: List[Mono[C, A]] = Nil) extends Mono[C, A]

case class Forall[+C, +A](as: List[A], body: Mono[C, A]) extends Poly[C, A]

object Forall {
  def apply[C, A](bound: List[Free[A]], body: Mono[C, A]): Poly[C, A] = {
    val as = bound.map(_.a)
    Forall(as, body bind bound)
  }
}

object Bound {
  var index = 0
  def fresh() = {
    index += 1
    Bound(index)
  }
}