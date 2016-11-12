package ulang.source

sealed trait Type
sealed trait Expr

case class Id(name: String) extends Expr with Type {
  override def toString = name
}

case class ExprApp(fun: Expr, args: List[Expr]) extends Expr {
  override def toString = (fun, args) match {
    case (Id(name), List(arg)) if Ops.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case (Id(name), List(arg)) if Ops.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case (Id(name), List(arg1, arg2)) if Ops.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case (Id(name), List(Lambda(bound, body))) if Ops.bindfix_ops contains name =>
      bound.mkString(name + " ", " ", ". " + body + ")")
    case (_, Nil) => fun.toString
    case _ => (fun :: args).mkString("(", " ", ")")
  }
}

case class Lambda(bound: List[Id], body: Expr) extends Expr {
  override def toString = bound.mkString("(λ ", " ", ". " + body + ")")
}

case class TypeApp(name: String, args: List[Type]) extends Type {
  override def toString = args match {
    case List(arg) if Cons.prefix_ops contains name =>
      "(" + name + " " + arg + ")"
    case List(arg) if Cons.postfix_ops contains name =>
      "(" + arg + " " + name + ")"
    case List(arg1, arg2) if Cons.infix_ops contains name =>
      "(" + arg1 + " " + name + " " + arg2 + ")"
    case Nil => name
    case _ => (name :: args).mkString("(", " ", ")")
  }
}

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
case class Defs(ops: List[Op], eqs: List[Expr]) extends Decl {
  override def toString = ops.mkString("definition\n  ", ";\n  ", ";\n") + eqs.mkString("where\n  ", ";\n  ", ";")
}

case class Axioms(ops: List[Op], axs: List[Expr]) extends Decl {
  override def toString = ops.mkString("axiomatization\n  ", ";\n  ", ";\n") + axs.mkString("where\n  ", ";\n  ", ";")
}

case class Module(decls: List[Decl]) {
  override def toString = decls.mkString("\n\n")
}