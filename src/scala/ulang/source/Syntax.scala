package ulang.source

import ulang.expr.Expr
import ulang.typ.Type

case class Constr(name: String, args: List[Type]) {
  override def toString = name + (if(args.isEmpty) "" else args.mkString(": ", " ", ""))
}

case class Op(name: String, typ: Type) {
  override def toString = name + ": " + typ
}

sealed trait Decl

case class Imports(names: List[String]) extends Decl {
  override def toString = names.mkString("import ", " ", ";")
}

case class Types(types: List[(Type,Type)]) extends Decl {
  override def toString = types.map {
    case (lhs, rhs) => lhs + " = " + rhs
  } mkString("types\n  ", ";\n  ", ";")
}

case class Datas(datas: List[(Type, List[Constr])]) extends Decl {
  override def toString = datas.map {
    case (lhs, constrs) => lhs + " = " + constrs.mkString(" | ")
  } mkString("types\n  ", ";\n  ", ";")
}

// TODO: empty list has semicolon
case class Defs(ops: List[Op], eqs: List[Expr[String]]) extends Decl {
  override def toString = ops.mkString("definition\n  ", ";\n  ", ";\n") + eqs.mkString("where\n  ", ";\n  ", ";")
}

case class Axioms(ops: List[Op], axs: List[Expr[String]]) extends Decl {
  override def toString = ops.mkString("axiomatization\n  ", ";\n  ", ";\n") + axs.mkString("where\n  ", ";\n  ", ";")
}

case class Module(decls: List[Decl]) {
  override def toString = decls.mkString("\n\n")
}