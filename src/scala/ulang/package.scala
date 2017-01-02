import scala.annotation.tailrec

package object ulang {
  @tailrec
  def fix[A](i: A, f: A => A): A = {
    val j = f(i)
    if (i == j) j else fix(j, f)
  }
  
  def id[A] = (a: A) => a

  def fatal(msg: String) = sys.error(msg)
  def error(msg: String) = sys.error(msg)
}