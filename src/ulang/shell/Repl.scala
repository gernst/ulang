package ulang.shell

import ulang.Shell
import ulang.Context

import arse.control._
import ulang.source._
import ulang.syntax._
import ulang.semantics._
import ulang.transform._

object Repl extends Shell {
  var context: Context = _
  val prompt = "> "

  def clear() {
    context = Context.default rename "shell"
  }

  clear()
  // read("import bool")
  // read("import nat")

  val commands = Map(
    ":sig" -> cmd(out(context.sig)),
    ":defs" -> cmd(out(context.df)),
    ":syntax" -> cmd(out(context.syntax)),
    ":net" -> cmd(out(context.df.net)),
    ":model" -> cmd(out(context.model.keys.mkString(" "))),
    ":prove" -> cmd(Prove(context).repl),
    ":clear" -> cmd(clear()))

  def read(line: String) = {
    import ulang.Parsers._
    import Parsers._
    import Reorder._
    import Convert._
    import Infer._
    import Eval._
    import Model._
    import ulang.syntax.predefined.pred.Eq

    val parse = decl $
    val res = parse(tokenize(line))

    res match {
      case OpDef(_expr) =>
        val (expr, typ) = context toExpr _expr
        expr match {
          case Eq(_, _) =>
            context += expr
          case _ =>
            val v = eval(expr, context.model)
            out("  = " + v + " : " + typ)
        }
      case _ =>
        context += res
        out("defined " + res)
    }
  }
}