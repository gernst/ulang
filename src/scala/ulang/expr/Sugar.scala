package ulang.expr

class Unary[A](val op: Free[A]) extends (Expr[A] => Expr[A]) {
  def unapply(e: Expr[A]) = e match {
    case App(`op`, arg) =>
      Some(arg)
    case _ =>
      None
  }

  def apply(arg: Expr[A]) = {
    App(op, arg)
  }
}

class Binary[A](val op: Free[A]) extends ((Expr[A], Expr[A]) => Expr[A]) {
  def unapply(e: Expr[A]) = e match {
    case App(App(`op`, arg1), arg2) =>
      Some((arg1, arg2))
    case _ =>
      None
  }

  def apply(arg1: Expr[A], arg2: Expr[A]) = {
    App(App(op, arg1), arg2)
  }
}

class Ternary[A](val op: Free[A]) extends ((Expr[A], Expr[A], Expr[A]) => Expr[A]) {
  def unapply(e: Expr[A]) = e match {
    case App(App(App(`op`, arg1), arg2), arg3) =>
      Some((arg1, arg2, arg3))
    case _ =>
      None
  }

  def apply(arg1: Expr[A], arg2: Expr[A], arg3: Expr[A]) = {
    App(App(App(op, arg1), arg2), arg3)
  }
}

class Nary[A](val op: Free[A], val neutral: Free[A]) {
  def flatArgs(e: Expr[A]): List[Expr[A]] = e match {
    case App(App(`op`, arg1), arg2) =>
      flatArgs(arg1) ++ flatArgs(arg2)
    case _ =>
      List(e)
  }

  def unapply(e: Expr[A]) = flatArgs(e) match {
    case List(_) => None
    case args    => Some(args)
  }

  def apply(args: List[Expr[A]]): Expr[A] = {
    if (args.isEmpty) neutral
    else args.reduce((arg1, arg2) => App(App(op, arg1), arg2))
  }
}

class Binder[A](val op: Free[A]) extends ((Free[A], Expr[A]) => Expr[A]) {
  def apply(bound: Free[A], body: Expr[A]): Expr[A] = {
    Bind(op, bound, body)
  }

  def unapply(expr: Expr[A]): Option[(Free[A], Expr[A])] = expr match {
    case Bind(`op`, bound, body) =>
      Some((bound, body))
    case _ =>
      None
  }
}

object Bind {
  def apply[A](op: Free[A], bound: Free[A], body: Expr[A]): Expr[A] = {
    App(op, Abs(op, body))
  }

  def unapply[A](expr: Expr[A]): Option[(Free[A], Free[A], Expr[A])] = expr match {
    case App(op: Free[A], Lambda(bound, body)) =>
      Some((op, Free(bound), body))
    case _ =>
      None
  }
}

object Apps {
  def apply[A](fun: Expr[A], args: List[Expr[A]]): Expr[A] = {
    args.foldLeft(fun)(App(_,_))
  }

  def unapply[A](expr: Expr[A]): Option[(Expr[A], List[Expr[A]])] = {
    Some(flatten(expr, Nil))
  }

  def flatten[A](expr: Expr[A], args: List[Expr[A]]): (Expr[A], List[Expr[A]]) = expr match {
    case App(fun, arg) =>
      flatten(fun, arg :: args)
    case _ =>
      (expr, args)
  }
}

object Abss {
  def apply[A](bounds: List[Free[A]], body: Expr[A]): Expr[A] = {
    bounds.foldRight(body)(Abs(_,_))
  }
/*
  def unapply[A](expr: Expr[A]): Option[(List[Expr[A]], Expr[A])] = {
    Some(flatten(expr))
  }

  def flatten[A](expr: Expr[A]): (List[Expr[A]], Expr[A]) = expr match {
    case Lambda(bound, body) =>
      val (bounds, inner) = flatten(body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
  */
}

/*
object Binds extends ((Free[A], List[FreeVar], Expr[A]) => Expr[A]) {
  def apply(op: Free[A], bounds: List[FreeVar], body: Expr[A]): Expr[A] = {
    bounds.foldRight(body)(Bind(op, _, _))
  }

  def unapply(expr: Expr[A]): Option[(Free[A], List[Expr[A]], Expr[A])] = expr match {
    case Bind(op, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      Some((op, bound :: bounds, inner))
    case _ =>
      None
  }

  def flatten(op: Free[A], expr: Expr[A]): (List[Expr[A]], Expr[A]) = expr match {
    case Bind(`op`, bound, body) =>
      val (bounds, inner) = flatten(op, body)
      (bound :: bounds, inner)
    case _ =>
      (Nil, expr)
  }
}
*/