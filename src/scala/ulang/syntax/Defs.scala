package ulang.syntax

import ulang.expr._
import ulang.typ._

case class Defs(data: Map[Con, Set[Op]], syn: Map[Con, (List[Param], Type)], axioms: List[Expr]) {
  lazy val constrs = data.flatMap(_._2)

  /*
  lazy val ops: Map[Op, Expr] = {
    val dfs = axioms map {
      case Eq(Apps(op: Op, args), rhs) =>
        (op, args, rhs)
    }

    val grouped = dfs.groupBy(_._1)

    val folded = grouped.map {
      case (op, axs) =>
        val cases = axs map {
          case (_, args, rhs) => Case(args, rhs)
        }
        (op, Match(cases))
    }

    folded.toMap
  }

  private def _synonym(typ: TypeApp): Type = syn get typ.con match {
    case Some((params, rhs)) =>
      val theta = (params zip typ.args).toMap
      rhs subst theta
    case None =>
      typ
  }

  def synonym(typ: Type): Type = typ match {
    case TypeApp(name, args) =>
      val res = _synonym(TypeApp(name, args map synonym))
      if (res != typ) synonym(res) /* until fixpoint */
      else typ
    /*case TypeInst(orig, gen) =>
      TypeInst(orig, synonym(gen)) */
    case _ =>
      typ
  }
  */

  override def toString = {
    val ss = syn map { case (con, (params, rhs)) => "type " + con + params.mkString(" ", " ", " = ") + rhs }
    val ds = data map { case (con, ops) => "data " + con + ops.mkString(" = ", " | ", "") }
    val os = axioms map { _.toString }
    (ss ++ ds ++ os).mkString("\n")
  }
}

object Defs {
  val empty = Defs(Map.empty[Con, Set[Op]], Map.empty[Con, (List[Param], Type)], Nil)
  val default = empty

  def apply(data: List[(Con, Set[Op])], syn: List[(Con, (List[Param], Type))], axioms: List[Expr]): Defs = {
    Defs(data.toMap, syn.toMap, axioms)
  }
}