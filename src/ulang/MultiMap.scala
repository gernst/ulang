package ulang

class MultiMap[A, B](m: Map[A, List[B]]) {
  def insert(ab: (A, B)) = {
    val (a, b) = ab
    m.get(a) match {
      case None => m + (a -> List(b))
      case Some(bs) => m + (a -> (b :: bs))
    }
  }

  def merge(n: Map[A, List[B]]): Map[A, List[B]] = {
    val as = m.keys ++ n.keys
    val ps = as map {
      a =>
        val bsm = m.getOrElse(a, Nil)
        val bsn = n.getOrElse(a, Nil)
        (a, bsm ::: bsn)
    }
    ps.toMap
  }
  
  def assoc = {
    
  }
}

object MultiMap {
  implicit def toMultiMap[A, B](m: Map[A, List[B]]) = new MultiMap(m)

  def empty[A, B] = Map.empty[A, List[B]]

  def apply[A, B](ps: Iterable[(A, B)]): Map[A, List[B]] = {
    ps.foldLeft(empty[A, B])(_ insert _)
  }
}