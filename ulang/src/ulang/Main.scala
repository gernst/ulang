package ulang

class Error(msg: String) extends Exception(msg)

// the only mutable state, a reference to an immutable value
class Ref[A](var get: A)

object Main:
  // the modules of the library are resources, next to the prelude
  def library(name: String): String =
    try os.read(os.resource / s"$name.u")
    catch
      case _: os.ResourceNotFoundException =>
        throw new Error(s"parse error: unknown module $name")

  // the commands of the prelude, and the scope with its notations and modules
  val (prelude, scope) =
    Resolve.cmds(Scope(Set(), Nil, Set("prelude"), library), source.Parser.parse(library("prelude")).cmds)

  // runs the text of file after the prelude, inlining its imports
  def run(text: String, file: os.Path): List[String] =
    val s = scope.copy(loaded = scope.loaded + file.baseName, read = module(file / os.up))
    val (cmds, _) = Resolve.cmds(s, source.Parser.parse(text).cmds)
    new core.Run(core.Check.program(core.Program(prelude ++ cmds))).run()

  // modules are files in the directory of the importing file, or in the library
  def module(dir: os.Path)(name: String): String =
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
