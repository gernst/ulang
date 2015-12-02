package ulang.semantics

trait Data

case class DataApp(fun: Data, arg: Any) extends Data {
  override def toString = "(" + fun + " " + arg + ")" 
}
