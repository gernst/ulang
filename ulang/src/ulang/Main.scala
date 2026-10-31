package ulang

class Error(msg: String) extends Exception(msg)

// the only mutable state, a reference to an immutable value
class Ref[A](var get: A)

object Main:
  // the modules of the library are resources, next to the prelude
  def library(name: Name): String =
    try os.read(os.resource / s"$name.u")
    catch
      case _: os.ResourceNotFoundException =>
        throw new Error(s"parse error: unknown module $name")

  val prelude: Program =
    Parser.parseProgram(library("prelude"), Nil, Set("prelude"), library)

  // runs the text of file after the prelude, whose notations and modules apply to it, inlining its imports
  def run(text: String, file: os.Path): List[String] =
    val loaded = Parser.imports(prelude) + file.baseName
    val program = Parser.parseProgram(text, Parser.notations(prelude), loaded, module(file / os.up))
    new Run(Check.program(Program(prelude.cmds ++ program.cmds))).run()

  // modules are files in the directory of the importing file, or in the library
  def module(dir: os.Path)(name: Name): String =
    if os.isFile(dir / s"$name.u")
    then os.read(dir / s"$name.u")
    else library(name)

  // prints the output of file and whether it succeeded
  def report(file: String): Boolean =
    try
      val path = os.Path(file, os.pwd)
      val lines = run(os.read(path), path)
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
