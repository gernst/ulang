package ulang.expr

sealed trait Expr[+A] {
  override def toString = format(Nil)

  def format[A1 >: A](s: List[A1]): String = this match {
    case Bound(n) => s(n).toString
    case Free(a) => a.toString
    case App(fun, arg) => "(" + (fun format s) + " " + (arg format s) + ")"
    // case Apps(fun, args) => (fun :: args).map(_ format s).mkString("(", " ", ")")
    case Lambda(a, body) => body format (a :: s)
  }

  def bind[A1 >: A](e: Expr[A1], i: Int = 0): Expr[A1] = this match {
    case Bound(_) => this
    case `e` => Bound(i)
    case Free(b) => Free(b)
    case App(fun, arg) => App(fun bind (e, i), arg bind (e, i))
    case Lambda(a, body) => Lambda(a, body bind (e, i + 1))
  }

  def unbind[A1 >: A](e: Expr[A1], i: Int = 0): Expr[A1] = this match {
    case Bound(`i`) => e
    case Bound(_) => this
    case Free(a) => Free(a)
    case App(fun, arg) => App(fun unbind (e, i), arg unbind (e, i))
    case Lambda(a, body) => Lambda(a, body unbind (e, i + 1))
  }

  def rename[B](r: A => B): Expr[B] = this match {
    case Bound(n) => Bound(n)
    case Free(a) => Free(r(a))
    case App(fun, arg) => App(fun rename r, arg rename r)
    case Lambda(a, body) => Lambda(r(a), body rename r)
  }

  def subst[A1 >: A](s: A1 => Expr[A1]): Expr[A1] = this match {
    case Bound(n) => this
    case Free(a) => s(a)
    case App(fun, arg) => App(fun subst s, arg subst s)
    case Lambda(a, body) => Lambda(a, body subst s)
  }

  def eval(s: List[Value], m: A => Value): Value = this match {
    case Bound(n) => s(n)
    case Free(a) => m(a)
    case App(fun, arg) =>
      val f = fun eval (s, m)
      val a = arg eval (s, m)
      f match {
        case f: Function @unchecked =>
          f(a)
      }
    case Lambda(a, body) =>
      (x: Value) => body eval (x :: s, m)
  }

  def reduce[A1 >: A](m: A1 => Option[Expr[A1]]): Expr[A1] = this match {
    case Bound(_) => this
    case Free(a) =>
      m(a) match {
        case Some(b) => b reduce m
        case None => this
      }
    case App(fun, arg) =>
      val f = fun reduce m
      val a = arg reduce m
      f match {
        case Lambda(_, body) =>
          body unbind a
        case _ =>
          App(f, a)
      }
    case Lambda(a, body) =>
      Lambda(a, body reduce m)
  }
}

case class Bound(n: Int) extends Expr[Nothing]

case class Free[+A](a: A) extends Expr[A] {
  def in[A1 >: A](e: Expr[A1]): Boolean = e match {
    case Bound(_) => false
    case Free(_) => e == this
    case App(fun, arg) => (this in fun) || (this in arg)
    case Lambda(a, body) => this in body
  }
}

case class App[+A](fun: Expr[A], arg: Expr[A]) extends Expr[A]
case class Lambda[+A](a: A, body: Expr[A]) extends Expr[A]