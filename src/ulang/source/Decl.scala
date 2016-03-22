package ulang.source

import arse.Fixity
import ulang.syntax.Con

sealed trait Decl
case class Import(mod: Module) extends Decl
case class FixDecl(fixity: Fixity, name: String) extends Decl
case class TypeDecl(con: Con) extends Decl
case class OpDecl(name: String, typ: Type) extends Decl
case class DataDef(typ: Schema, constrs: List[OpDecl]) extends Decl
case class TypeDef(lhs: Schema, rhs: Type) extends Decl
case class OpDef(op: String, args: List[Expr], rhs: Expr) extends Decl