package ulang.source

import arse.Fixity
import ulang.syntax.Thy

sealed trait Decl
case class Import(thy: Thy) extends Decl
case class FixDecl(fixity: Fixity, name: String) extends Decl
case class OpDecl(name: String, typ: Type) extends Decl
case class DataDef(typ: Type, constrs: List[OpDecl]) extends Decl
case class TypeDef(lhs: Type, rhs: Type) extends Decl
case class OpDef(axiom: Expr) extends Decl

object TypeDecl {
  def unapply(decl: Decl): Option[Type] = decl match {
    case DataDef(typ, _) => Some(typ)
    case TypeDef(lhs, _) => Some(lhs)
    case _ => None
  }
}