package ulang

class Error(msg: String) extends Exception(msg)

// the only mutable state, a reference to an immutable value
class Ref[A](var get: A)

object Main:
  val prelude: Program =
    Parser.parseProgram(os.read(os.resource / "prelude.u"))

  // runs text after the prelude
  def run(text: String): List[String] =
    new Run(Check.program(Program(prelude.cmds ++ Parser.parseProgram(text).cmds))).run()

  // prints the output of file and whether it succeeded
  def report(file: String): Boolean =
    try
      val lines = run(os.read(os.Path(file, os.pwd)))
      for line <- lines do
        println(line)
      true
    catch
      case e: Error =>
        System.err.println(s"$file: ${e.getMessage}")
        false

  def main(args: Array[String]): Unit =
    val results = args.toList.map(report)
    if results.contains(false) then
      sys.exit(1)
