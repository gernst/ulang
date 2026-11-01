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
  def library(name: Id): String =
    try
      os.read(os.resource / s"$name.u")
    catch
      case _: os.ResourceNotFoundException =>
        error("parse", s"unknown module $name")

  val prelude: Program = Parser.parseProgram(library("prelude"), Scope(Set(), Nil, loading = Set("prelude"), read = library))

  // the notations and modules of the prelude, which every module sees
  val preludeScope: Scope = Parser.after(prelude.cmds, Scope(Set(), Nil))

  // the scope of module name, which is parsed after the prelude with its notations, and reads modules with read
  def scope(name: Id, read: Id => String): Scope =
    Scope(Set(), preludeScope.notations, preludeScope.modules, Set(name), Some(preludeScope.notations), read)

  // the text of file, to which the notations and modules of the prelude apply
  def parse(text: String, file: os.Path): Program =
    Parser.parseProgram(text, scope(file.baseName, module(file / os.up)))

  // the checked text of file after the prelude, which is the module of its base name, where a module of the library
  // that the prelude imports keeps only its evals, runs, and tests, since the prelude declares the rest
  def load(text: String, file: os.Path, args: List[String] = Nil): Run =
    val cmds = parse(text, file).cmds
    val name = file.baseName
    val own = if (preludeScope.modules.contains(name) || name == "prelude") && text == library(name) then runnable(cmds) else cmds
    new Run(Check.program(Program(Cmd.Module("prelude", prelude.cmds)() :: own), name), file / os.up, args)

  def runnable(cmds: List[Cmd]): List[Cmd] =
    cmds match
      case Nil =>
        Nil
      case (c: (Cmd.Eval | Cmd.Run | Cmd.Test)) :: rest =>
        c :: runnable(rest)
      case _ :: rest =>
        runnable(rest)

  def run(text: String, file: os.Path, args: List[String] = Nil): List[String] =
    deep(load(text, file, args).run())

  // f in a thread with a large stack, since the interpreter recurses as deeply as the programs it runs
  def deep[A](f: => A): A =
    val result = Ref[Option[Either[Throwable, A]]](None)
    val thread = Thread(null, () => result.get = Some(try Right(f) catch case e: Throwable => Left(e)), "ulang", 1L << 30)
    thread.start()
    thread.join()
    result.get match
      case Some(Right(a)) =>
        a
      case Some(Left(e)) =>
        throw e
      case None =>
        error("runtime", "the program did not finish")

  // the runs and tests of a program
  def blocks(r: Run): List[Cmd] =
    r.program.cmds.filter:
      case Cmd.Run(_, _) | Cmd.Test(_, _) =>
        true
      case _ =>
        false

  // modules are files in the directory of the importing file, or in the library
  def module(dir: os.Path)(name: Id): String =
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
  def report(file: String, args: List[String]): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      for line <- run(os.read(path), path, args) do
        println(line)
      true

  // runs each test of file on its own, also after a failed one, printing whether it passed
  def tests(file: String, args: List[String]): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      val r = load(os.read(path), path, args)
      val results =
        for case c @ Cmd.Test(name, _) <- blocks(r)
        yield
          try
            r.output(c)
            println(s"test $name passed")
            true
          catch
            case e: Error =>
              println(s"test $name failed: ${e.getMessage}")
              false
      results.forall(identity)

  // the run or test of that name
  def block(r: Run, which: String): Cmd =
    val named =
      blocks(r).collectFirst:
        case c @ Cmd.Run(name, _) if name == which =>
          c
        case c @ Cmd.Test(name, _) if name == which =>
          c
    named.getOrElse(error("runtime", s"there is no run or test $which"))

  // runs one run or test of file
  def only(which: String, file: String, args: List[String]): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      val r = load(os.read(path), path, args)
      val c = block(r, which)
      for line <- r.output(c) do
        println(line)
      true

  // ulang file.u ..., ulang --tests file.u ..., or ulang --only name file.u, each followed by -- and the arguments of os_args
  def main(args: Array[String]): Unit =
    val (options, rest) = args.toList.span(_ != "--")
    val arguments = rest.drop(1)
    val results = deep:
      options match
        case "--tests" :: files =>
          files.map(tests(_, arguments))
        case "--only" :: n :: List(file) =>
          List(only(n, file, arguments))
        case files =>
          files.map(report(_, arguments))
    if results.contains(false) then
      sys.exit(1)
