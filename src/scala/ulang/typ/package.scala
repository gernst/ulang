package ulang

package object typ {
  type Type = Mono[String, String]
  type Param = Free[String]

  val bool = App("Bool")

  val alpha = Free("α")
  val beta = Free("β")
  val gamma = Free("γ")

  object Function extends Binary("→")
  object Product extends Binary("×")

  type Carrier = (Any => Boolean)
  type Relation = (List[Any] => Boolean)

  type Subst[C, A] = A => Mono[C, A]

  object Subst {
    def empty[C, A]: Subst[C, A] = (a: A) => Free(a)
    def singleton[A, C](a: A, t: Mono[C, A]) = (b: A) => if (a == b) t else Free(b)
  }

  implicit class SubstOps[C, A](s: A => Mono[C, A]) {
    def o(t: A => Mono[C, A]) = (a: A) => s(a) subst t
  }

  implicit class TypeOps(t: Type) {
    def →(r: Type): Type = Function(t, r)
  }
}