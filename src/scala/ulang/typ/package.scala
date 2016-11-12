package ulang

package object typ {
  type Alpha = Bound
  type Param = Free[String]

  object Alpha {
    def fresh(): Alpha = ???
  }

  type Type = Mono[String, String]

  val bool = App("Bool")

  val alpha = Free("α")
  val beta = Free("β")
  val gamma = Free("γ")

  object Function extends Binary("→")
  object Product extends Binary("×")

  type Carrier = (Any => Boolean)
  type Relation = (List[Any] => Boolean)
  
  object Subst {
    def empty[C,A]: (A => Mono[C,A]) = (a: A) => Free(a)
    def singleton[A,C](a: A, t: Mono[C,A])= (b: A) => if(a == b) t else Free(b)
  }

  implicit class SubstOps[C, A](s: A => Mono[C, A]) {
    def o(t: A => Mono[C, A]) = (a: A) => s(a) subst t
  }
  
  implicit class TypeOps(t: Type) {
    def →(r: Type): Type = Function(t, r)
  }
}