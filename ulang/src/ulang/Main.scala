package ulang

class Error(msg: String) extends Exception(msg)

// the only mutable state, a reference to an immutable value
class Ref[A](var get: A)

object Main:
  val prelude: Program =
    Parser.parseProgram(os.read(os.resource / "prelude.u"), Nil)

  // runs text after the prelude, whose notations apply to text
  def run(text: String): List[String] =
    val program = Parser.parseProgram(text, Parser.notations(prelude))
    new Run(Check.program(Program(prelude.cmds ++ program.cmds))).run()

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
