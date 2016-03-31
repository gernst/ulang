package ulang.source

import arse.Fixity

sealed trait Decl
case class Import(mod: Module) extends Decl
case class FixDecl(fixity: Fixity, name: String) extends Decl
case class OpDecl(name: String, typ: Type) extends Decl
case class DataDef(typ: Type, constrs: List[OpDecl]) extends Decl
case class TypeDef(lhs: Type, rhs: Type) extends Decl
case class OpDef(axiom: Expr) extends Decl