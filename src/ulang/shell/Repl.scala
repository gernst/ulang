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
    context = Context.default
  }

  clear()

  val commands = Map(
    ":sig" -> cmd(out(context.thy.sig)),
    ":defs" -> cmd(out(context.thy.df)),
    ":syntax" -> cmd(out(context.thy.syntax)),
    // ":net" -> cmd(out(context.thy.df.net)),
    // ":model" -> cmd(out(context.model.keys.mkString(" "))),
    // ":prove" -> cmd(Prove(context).repl),
    ":clear" -> cmd(clear()))

  def read(line: String) = {
    import ulang.syntax.predefined._
    
/*
    {
      val decl = context.parse.decl(line)
      context += decl
    } or {
      val expr = context.parse.expr(line)
      out(expr)
    }*/
  }
}