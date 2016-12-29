package ulang.typ

case class Typing[C, A](ss: List[A => Mono[C, A]]) {
  def o(that: Typing[C, A]): Typing[C, A] = {
    Typing(for (s <- this.ss; t <- that.ss)
      yield s o t)
  }

  def unify(t1: Mono[C, A], t2: Mono[C, A]): Typing[C, A] = {
    Typing(ss.flatMap(s => (t1 subst s) unify (t2 subst s) map (s o _))) // XXX: code duplication
  }

  def hasFailed = ss.isEmpty
}

object Typing {
  def empty[C, A] = Typing[C, A](List(Subst.empty))
  def choose[C, A](a: A, ts: List[Mono[C, A]]): Typing[C, A] = ???
}