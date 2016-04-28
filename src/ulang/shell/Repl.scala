package ulang.shell

import ulang.Shell
import ulang.Context

import arse._
import ulang.source._
import ulang.syntax._
import ulang.semantics._
import ulang.transform._

object Repl extends Shell {
  var thy: Thy = _
  val prompt = "> "

  clear()

  def clear() {
    thy = Thy.default
  }

  val commands = Map(
    ":thy" -> cmd(out(thy)),
    ":sig" -> cmd(out(thy.sig)),
    ":defs" -> cmd(out(thy.df)),
    // ":net" -> cmd(out(context.thy.df.net)),
    // ":model" -> cmd(out(context.model.keys.mkString(" "))),
    ":prove" -> cmd(Prove(thy).repl),
    ":clear" -> cmd(clear()))

  def read(line: String) = {
    import ulang.syntax.predefined._
    import ulang.source.Parsers.decl

    {
      thy = decl(line, thy)
    } /*or {
      val expr = context.parse.expr(line)
      out(expr)
    }*/
  }
}