package ulang

import scala.io.StdIn

import arse._

abstract class Shell {
  def commands: Map[String, () => Any]
  def read(line: String): Unit
  def prompt: String

  def input(): String = input(prompt)
  def input(p: String): String = StdIn.readLine(p)

  def out(obj: Any) {
    Console.out.println(obj)
    Console.out.flush
  }

  def err(text: String) {
    Console.err.println(text)
    Console.err.flush
  }

  def cmd(c: => Any) = { () => c }

  def repl() {
    while (true) {
      try {
        input() match {
          case null =>
            out(":quit")
            return
          case ":quit" =>
            return
          case "" =>
          // 
          case line if commands contains line =>
            commands(line)()
          case line if line startsWith ":" =>
            error("in shell: unknown command " + line)
          case line =>
            read(line)
        }
      } catch {
        /*case e @ Error(msg) =>
          err("error: " + msg)
        // e.printStackTrace()
        case e @ Fatal(msg) =>
          err("fatal: " + msg)
          e.printStackTrace()*/
        case e: StackOverflowError =>
          err("fatal: stack overflow")
        case e: Throwable =>
          err("fatal: " + e.getMessage)
          e.printStackTrace()
      }
    }
  }

  def main(args: Array[String]) {
    repl()
  }
}