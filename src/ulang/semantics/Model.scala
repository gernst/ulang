package ulang.semantics

import arse._

import ulang._
import ulang.syntax._


object Model {
  import Eval._

  def equals(x: Any)(y: Any) = {
    if (!x.isInstanceOf[Data] || !y.isInstanceOf[Data])
      error("in eval: cannot compare " + x + " and " + y)
    if (x == y) True else False
  }

  def empty: Model = Map.empty
  def default: Model = Map(Eq.op -> equals _)

  def apply(df: Defs, init: Model): Model = {
    val cmodel = df.constrs.foldLeft(init) {
      case (m, op) =>
        m + (op -> constr(op, op.typ.arity))
    }

    val fmodel = df.ops.foldLeft(cmodel) {
      case (m, (op, rhs)) =>
        m + (op -> eval(rhs, Nil, Map.empty, m))
    }

    fmodel
  }

  def constr(data: Data, arity: Int): Any = {
    if (arity == 0) data
    else (v: Any) => constr(DataApp(data, v), arity - 1)
  }

  def function(self: Net, stack: List[Any], model: Model): Any = self.rhs match {
    case Nil => new Closure {
      def apply(arg: Any, model: Model) = {
        val (newstack, net) = pmatch(self, List(arg), stack)
        function(net, newstack, model)
      }
    }

    case List(rhs) =>
      eval(rhs, stack, Map.empty, model)

    case _ =>
      fatal("in eval: nondeterministic pattern match result: " + self.rhs)
  }

  def pmatch(self: Net, args: List[Any], stack: List[Any]): (List[Any], Net) = args match {
    case Nil => (stack, self)

    case (op: Op) :: rest if self.ops contains op =>
      pmatch(self.ops(op), rest, stack)

    case DataApp(fun, arg) :: rest if self.app.isDefined =>
      pmatch(self.app.get, fun :: arg :: rest, stack)

    case arg :: rest if self.bound.isDefined =>
      pmatch(self.bound.get, rest, arg :: stack)

    case _ =>
      fatal("in eval: pattern mismatch of " + args.mkString(" ") + " in\n" + self)
  }
}