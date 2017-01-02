package ulang.util

case class FoldLeft[A,B <: A](f: (A,A) => B) extends ((A,List[A]) => A) {
  def apply(b: A, as: List[A]): A = {
    as.foldLeft(b)(f)
  }
}

case class FoldRight[A, B <: A](f: (A,A) => B) extends ((List[A],A) => A) {
  def apply(as: List[A], b: A): A = {
    as.foldRight(b)(f)
  }
}