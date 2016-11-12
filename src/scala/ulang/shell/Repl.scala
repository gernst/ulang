package ulang.shell

import arse._

import ulang._
import ulang.source._
import ulang.syntax._
import ulang.semantics._

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
    /*import Parsers.decl
    
    {
      thy = decl(line, thy)
    } or {
      val expr = context.parse.expr(line)
      out(expr)
    }*/
  }
  
  def main(args: Array[String]) {
    repl()
  }
}