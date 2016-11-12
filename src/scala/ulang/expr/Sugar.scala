package ulang.expr

class Unary[A](val op: Free[A]) extends (Term[A] => Term[A]) {
  def unapply(e: Term[A]) = e match {
    case App(`op`, arg) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Term[A]) = {
    App(op, arg)
  }
}

class Binary[A](val op: Free[A]) extends ((Term[A], Term[A]) => Term[A]) {
  def unapply(e: Term[A]) = e match {
    case App(App(`op`, arg1), arg2) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Term[A], arg2: Term[A]) = {
    App(App(op, arg1), arg2)
  }
}

class Ternary[A](val op: Free[A]) extends ((Term[A], Term[A], Term[A]) => Term[A]) {
  def unapply(e: Term[A]) = e match {
    case App(App(App(`op`, arg1), arg2), arg3) =>
      Some((arg1, arg2, arg3))
    case _ =>
      None
  }

  def apply(arg1: Term[A], arg2: Term[A], arg3: Term[A]) = {
    App(App(App(op, arg1), arg2), arg3)
  }
}

class Nary[A](val op: Free[A], val neutral: Free[A]) {
  def flatArgs(e: Term[A]): List[Term[A]] = e match {
    case App(App(`op`, arg1), arg2) =>
      flatArgs(arg1) ++ flatArgs(arg2)
    case _ =>
      List(e)
  }

  def unapply(e: Term[A]) = flatArgs(e) match {
    case List(_) => None
    case args    => Some(args)
  }

  def apply(args: List[Term[A]]): Term[A] = {
    if (args.isEmpty) neutral
    else args.reduce((arg1, arg2) => App(App(op, arg1), arg2))
  }
}

class Binder[A](val op: Free[A]) extends ((Free[A], Term[A]) => Term[A]) {
  def apply(bound: Free[A], body: Term[A]): Term[A] = {
    Bind(op, bound, body)
  }

  def unapply(expr: Term[A]): Option[(Free[A], Term[A])] = expr match {
    case Bind(`op`, bound, body) =>
      Some((bound, body))
    case _ =>
      None
  }
}

object Bind {
  def apply[A](op: Free[A], bound: Free[A], body: Term[A]): Term[A] = {
    App(op, Lambda(op, body))
  }

  def unapply[A](expr: Term[A]): Option[(Free[A], Free[A], Term[A])] = expr match {
    case App(op: Free[A], Lambda(bound, body)) =>
      Some((op, Free(bound), body))
    case _ =>
      None
  }
}

/*
object Apps extends ((Term[A], List[Term[A]]) => Term[A]) {
  def apply(fun: Term[A], args: List[Term[A]]): Term[A] = {
    args.foldLeft(fun)(App)
  }

  def unapply(expr: Term[A]): Option[(Term[A], List[Term[A]])] = {
    Some(flatten(expr, Nil))
  }

  def flatten(expr: Term[A], args: List[Term[A]]): (Term[A], List[Term[A]]) = expr match {
    case App(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }
}

object Lambdas extends ((List[FreeVar], Term[A]) => Term[A]) {
  def apply(bounds: List[FreeVar], body: Term[A]): Term[A] = {
    bounds.foldRight(body)(Lambda)
  }

  def unapply(expr: Term[A]): Option[(List[Term[A]], Term[A])] = {
    Some(flatten(expr))
  }

  def flatten(expr: Term[A]): (List[Term[A]], Term[A]) = expr match {
    case Lambda(bound, body) =>
      val (bounds, inner) = flatten(body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}

object Binds extends ((Free[A], List[FreeVar], Term[A]) => Term[A]) {
  def apply(op: Free[A], bounds: List[FreeVar], body: Term[A]): Term[A] = {
    bounds.foldRight(body)(Bind(op, _, _))
  }

  def unapply(expr: Term[A]): Option[(Free[A], List[Term[A]], Term[A])] = expr match {
    case Bind(op, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      Some((op, bound :: bounds, inner))
    case _ =>
      None
  }

  def flatten(op: Free[A], expr: Term[A]): (List[Term[A]], Term[A]) = expr match {
    case Bind(`op`, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}
*/