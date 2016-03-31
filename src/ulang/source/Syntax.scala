package ulang.source

import arse._
import arse.control._

case object Bindfix extends Fixity
// case object Outfix extends Fixity

case class Syntax(
  prefix_ops: Map[String, Int],
  postfix_ops: Map[String, Int],
  infix_ops: Map[String, (Assoc, Int)],
  bindfix_ops: Set[String]) {

  def contains(name: String): Boolean = {
    ( /**/ (prefix_ops contains name)
      || (postfix_ops contains name)
      || (infix_ops contains name)
      || (bindfix_ops contains name))
  }

  def insert[A](m: Map[String, A], name_a: (String, A)) = {
    val (name, a) = name_a

    for (b <- m.get(name))
      if (a != b)
        error("illegal precedence overloading " + a + " and " + b + " for " + name)

    m + name_a
  }

  def ++(that: Syntax) = Syntax(
    that.prefix_ops.foldLeft(this.prefix_ops)(insert),
    that.postfix_ops.foldLeft(this.postfix_ops)(insert),
    that.infix_ops.foldLeft(this.infix_ops)(insert),
    that.bindfix_ops)

  def +(name: String, fixity: Fixity): Syntax = fixity match {
    case Prefix(prec) =>
      copy(prefix_ops = insert(prefix_ops, name -> prec))
    case Postfix(prec) =>
      copy(postfix_ops = insert(postfix_ops, name -> prec))
    case Infix(assoc, prec) =>
      copy(infix_ops = insert(infix_ops, name -> (assoc, prec)))
    case Bindfix =>
      copy(bindfix_ops = bindfix_ops + name)
    case _ =>
      fatal("in syntax: unknown mixfix declaration for " + name + ":" + fixity)
  }

  override def toString = {
    val s1 = prefix_ops.map { case (name, (prec)) => "prefix " + prec + " " + name }
    val s2 = postfix_ops.map { case (name, (prec)) => "postfix " + prec + " " + name }
    val s3 = infix_ops.map { case (name, (assoc, prec)) => "infix " + assoc + " " + prec + " " + name }
    val s4 = bindfix_ops.map { case name => "binder " + name }
    val ss = List(s1, s2, s3, s4).flatten
    ss.mkString("\n")
  }
}

object Syntax {
  val empty = Syntax(Map.empty, Map.empty, Map.empty, Set.empty)
  val default = empty + ("→",Infix(Right, 2)) + ("=", Infix(Non, 6))
}