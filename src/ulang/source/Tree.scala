package ulang.source

sealed trait Tree
/*
case class Num(n: BigInt) extends Tree
case class Str(s: String) extends Tree
*/
case class Id(name: String) extends Tree {
  override def toString = name
}

case class Node(args: List[Tree]) extends Tree {
  assert(!args.isEmpty)
  override def toString = args.mkString("(", " ", ")")
}