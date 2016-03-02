package ulang

import scala.annotation.tailrec

object Util {
  @tailrec
  def fix[A](i: A, f: A => A): A = {
    val j = f(i)
    if (i == j) j else fix(j, f)
  }
}