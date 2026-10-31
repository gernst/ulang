package ulang

class Error(msg: String) extends Exception(msg)

// errors of a phase, i.e., "parse", "type", or "runtime"
def error(phase: String, msg: String): Nothing =
  throw new Error(s"$phase error: $msg")

def error(phase: String, msg: String, pos: scala.util.parsing.input.Position): Nothing =
  error(phase, s"$msg at $pos")

// adds pos to the errors of f that have no position yet, such that errors are at the innermost node
def at[A](pos: Pos)(f: => A): A =
  try
    f
  catch
    case e: Error if pos != nowhere && !located(e.getMessage) =>
      throw new Error(s"${e.getMessage} at $pos")

def located(msg: String): Boolean =
  """.* at \d+\.\d+( in \w+)*""".r.matches(msg)

def require(phase: String, cond: Boolean, msg: => String): Unit =
  if !cond then
    error(phase, msg)

// the only mutable state, a reference to an immutable value
class Ref[A](var get: A)

object Main:
  // the modules of the library are resources, next to the prelude
  def library(name: Name): String =
    try
      os.read(os.resource / s"$name.u")
    catch
      case _: os.ResourceNotFoundException =>
        error("parse", s"unknown module $name")

  val prelude: Program = Parser.parseProgram(library("prelude"), Nil, Set("prelude"), library)

  // the text of file, to which the notations and modules of the prelude apply, inlining its imports
  def parse(text: String, file: os.Path): Program =
    val loaded = Parser.imports(prelude) + file.baseName
    Parser.parseProgram(text, Parser.notations(prelude), loaded, module(file / os.up))

  // the checked text of file after the prelude
  def load(text: String, file: os.Path): Run =
    new Run(Check.program(Program(prelude.cmds ++ parse(text, file).cmds)), file / os.up)

  def run(text: String, file: os.Path): List[String] =
    load(text, file).run()

  // the runs and tests of a program, which are numbered from 1 in the order of the file
  def blocks(r: Run): List[Cmd] =
    r.program.cmds.filter:
      case Cmd.Run(_) | Cmd.Test(_) =>
        true
      case _ =>
        false

  // modules are files in the directory of the importing file, or in the library
  def module(dir: os.Path)(name: Name): String =
    if os.isFile(dir / s"$name.u") then
      os.read(dir / s"$name.u")
    else
      library(name)

  // reports errors of file, and whether f succeeded
  def attempt(file: String)(f: => Boolean): Boolean =
    try
      f
    catch
      case e: Error =>
        System.err.println(s"$file: ${e.getMessage}")
        false

  // prints the output of file and whether it succeeded
  def report(file: String): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      for line <- run(os.read(path), path) do
        println(line)
      true

  // runs each test of file on its own, also after a failed one, printing whether it passed
  def tests(file: String): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      val r = load(os.read(path), path)
      val results =
        for case (c @ Cmd.Test(_), i) <- blocks(r).zip(LazyList.from(1))
        yield
          try
            r.output(c)
            println(s"test $i passed")
            true
          catch
            case e: Error =>
              println(s"test $i failed: ${e.getMessage}")
              false
      results.forall(identity)

  // runs the n-th run or test of file
  def only(n: Int, file: String): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      val r = load(os.read(path), path)
      val c = blocks(r).lift(n - 1).getOrElse(error("runtime", s"there is no run or test $n"))
      for line <- r.output(c) do
        println(line)
      true

  // ulang file.u ..., ulang --tests file.u ..., or ulang --only n file.u
  def main(args: Array[String]): Unit =
    val results =
      args.toList match
        case "--tests" :: files =>
          files.map(tests)
        case "--only" :: n :: List(file) =>
          List(only(n.toInt, file))
        case files =>
          files.map(report)
    if results.contains(false) then
      sys.exit(1)
