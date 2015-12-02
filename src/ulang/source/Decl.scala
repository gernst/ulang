package ulang.source

import arse.Fixity

sealed trait Decl
case class Import(name: String) extends Decl
case class FixDecl(fixity: Fixity, op: String) extends Decl
case class DataDef(typ: Tree, constrs: List[OpDecl]) extends Decl
case class TypeDef(lhs: Tree, rhs: Option[Tree]) extends Decl
case class OpDecl(name: String, typ: Tree) extends Decl
case class OpDef(expr: Tree) extends Decl
case class TheoremDef(expr: Tree) extends Decl
case class Incomplete(data: Any) extends Decl