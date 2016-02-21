package ulang.syntax

case class Defs(data: Map[Con, Set[Op]], syn: Map[Con, (List[TypeParam], Type)], axioms: List[Expr]) {
  def +(con: Con, params: List[TypeParam], rhs: Type) = copy(syn = syn + (con -> (params, rhs)))
  def +(con: Con, constrs: Set[Op]) = copy(data = data + (con -> constrs))
  // def +(op: Op, rhs: Expr) = copy(ops = ops + (op -> rhs)) 
  def +(ax: Expr) = copy(axioms = axioms :+ ax)
  def ++(that: Defs) = Defs(this.data ++ that.data, this.syn ++ that.syn, this.axioms ++ that.axioms)

  override def toString = {
    val ss = syn map { case (con, (params, rhs)) => "type " + con + params.mkString(" ") + " = " + rhs }
    val ds = data map { case (con, ops) => "data " + con + ops.mkString(" = ", " | ", "") }
    val os = axioms map { _.toString }
    (ss ++ ds ++ os).mkString("\n")
  }

  lazy val constrs = data.flatMap(_._2)

  lazy val ops: Map[Op, Expr] = {
    import ulang.syntax.predefined.pred

    val dfs = axioms map {
      case pred.Eq(FlatApp(op, args), rhs) =>
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
    case TypeInst(orig, gen) =>
      TypeInst(orig, synonym(gen))
    case _ =>
      typ
  }
}

object Defs {
  val empty = Defs(Map.empty, Map.empty, List.empty)
  val default = empty
}