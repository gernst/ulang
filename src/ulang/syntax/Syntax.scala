package ulang.syntax

import arse._
import arse.control._
import ulang.source._

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

  def ++(decls: List[Decl]): Syntax = {
    decls.foldLeft(this)(_ + _)
  }

  def +(decl: Decl): Syntax = decl match {
    case Import(thy) =>
      this ++ thy.syntax
    case FixDecl(Prefix(prec), name) =>
      copy(prefix_ops = insert(prefix_ops, name -> prec))
    case FixDecl(Postfix(prec), name) =>
      copy(postfix_ops = insert(postfix_ops, name -> prec))
    case FixDecl(Infix(assoc, prec), name) =>
      copy(infix_ops = insert(infix_ops, name -> (assoc, prec)))
    case FixDecl(Bindfix, name) =>
      copy(bindfix_ops = bindfix_ops + name)
    case _ =>
      // fatal("in syntax: unknown mixfix declaration " + fd)
      this
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
  def apply(decls: List[Decl]): Syntax = empty ++ decls

  val empty = Syntax(Map.empty, Map.empty, Map.empty, Set.empty)
  val arrow = FixDecl(Infix(Right, 2), "→")
  val equals = FixDecl(Infix(Non, 6), "=")
  val default = empty + arrow + equals
}