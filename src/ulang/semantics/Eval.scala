package ulang.semantics

import arse._
import ulang.syntax._
import ulang.syntax.predefined._

object Eval {
  type Stack = List[Any]
  type State = Map[FreeVar, Any]
  type Model = Map[Op, Any]

  trait Closure extends ((Any, Model) => Any)

  def apply(fun: Any, arg: Any, model: Model): Any = fun match {
    case f: (Any => Any) @unchecked => f(arg)
    case f: Closure => f(arg, model)
    case _ => fatal("in apply: " + fun + " is not a function")
  }

  def eval(self: Expr, model: Model): Any = eval(self, Nil, Map.empty, model)

  def eval(self: Expr, stack: Stack, state: State, model: Model): Any = self match {
    case BoundVar(index) =>
      if (index < stack.length)
        stack(index)
      else
        fatal("in eval: unbound index " + index)

    case fv: FreeVar =>
      state.getOrElse(fv, fatal("in eval: unbound var " + fv))

    case pred.IfThenElse(test, arg1, arg2) =>
      eval(test, stack, state, model) match {
        case prop.True =>
          eval(arg1, stack, state, model)
        case prop.False =>
          eval(arg2, stack, state, model)
        case v =>
          fatal("in eval: non-boolean result of test " + v)
      }

    case op: Op =>
      model.getOrElse(op, fatal("in eval: unbound op " + op))

    case App(fun, arg) =>
      val funv = eval(fun, stack, state, model)
      val argv = eval(arg, stack, state, model)
      apply(funv, argv, model)

    case Lambda(bound, body) =>
      { (arg: Any) => eval(body, arg :: stack, state, model) }

    case Match(cases) =>
      val net = Net(cases)
      ???
  }
}