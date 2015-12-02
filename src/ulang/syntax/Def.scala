package ulang.syntax

case class Def(data: Map[Con, Set[Op]], syn: Map[Con, (List[TypeParam], Type)], axioms: List[Expr]) {
  def +(con: Con, params: List[TypeParam], rhs: Type) = copy(syn = syn + (con -> (params, rhs)))
  def +(con: Con, constrs: Set[Op]) = copy(data = data + (con -> constrs))
  def +(ax: Expr) = copy(axioms = axioms :+ ax)
  def ++(that: Def) = {
    val _axioms = (this.axioms ++ that.axioms).distinct
    Def(this.data ++ that.data, this.syn ++ that.syn, _axioms)
  }

  override def toString = {
    val ss = syn map { case (con, (params, rhs)) => "type " + con + params.mkString(" ") + " = " + rhs }
    val ds = data map { case (con, ops) => "data " + con + ops.mkString(" = ", " | ", "") }
    val os = axioms
    (ss ++ ds ++ os).mkString("\n")
  }

  lazy val constrs = data.flatMap(_._2)

  lazy val defs = axioms collect {
    case predefined.pred.Eq(FlatApp(op, args), rhs) =>
      (op, (args, rhs))
  }

  lazy val nets = {
    val ops = defs.map(_._1).distinct
    ops.map {
      op =>
        val cases = defs.collect { case (`op`, args_expr) => args_expr }
        (op -> Net(cases))
    }
  }

  // careful: source order of definitions is messed up
  lazy val net = Net(nets.toMap, None, None, Nil)

  /*
  private def _synonyms(typ: TypeApp) = {
    val candidates = syn map { case (lhs, rhs) => (lhs ~ typ, rhs) }
    val ts = candidates collect { case (Some(theta), rhs) => rhs subst theta }
    if (ts.isEmpty) List(typ)
    else ts
  }
  */

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

object Def {
  val empty = Def(Map.empty, Map.empty, List.empty)
  val default = empty
}