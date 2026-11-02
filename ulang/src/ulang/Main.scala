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
  // the directory of the library, given by the property ulang.lib or the variable ULANG_LIB,
  // or else None, when the library is read from the resources
  val libraryDir: Option[os.Path] =
    sys.props.get("ulang.lib") match
      case Some(dir) =>
        Some(os.Path(dir, os.pwd))
      case None =>
        sys.env.get("ULANG_LIB").map(os.Path(_, os.pwd))

  // the text of a module of the library, which is next to the prelude
  def libraryText(name: Id): Option[String] =
    libraryDir match
      case Some(dir) =>
        Option.when(os.isFile(dir / s"$name.u"))(os.read(dir / s"$name.u"))
      case None =>
        try Some(os.read(os.resource / s"$name.u"))
        catch case _: os.ResourceNotFoundException => None

  def library(name: Id): String =
    libraryText(name).getOrElse(error("parse", s"unknown module $name"))

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
  // that the prelude imports keeps only its runs and tests, since the prelude declares the rest
  def load(text: String, file: os.Path, args: List[String] = Nil): Run =
    val cmds = parse(text, file).cmds
    val name = file.baseName
    val own = if (preludeScope.modules.contains(name) || name == "prelude") && text == library(name) then runnable(cmds) else cmds
    new Run(Check.program(Program(Cmd.Module("prelude", prelude.cmds)() :: own), name), file / os.up, args)

  def runnable(cmds: List[Cmd]): List[Cmd] =
    cmds match
      case Nil =>
        Nil
      case (c: Cmd.Test) :: rest =>
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

  // the tests of a program
  def blocks(r: Run): List[Cmd] =
    r.program.cmds.filter:
      case Cmd.Test(_, _, _) =>
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

  // calls the procedure name of file, which takes no parameters, and prints what it prints
  def runs(file: String, name: String, args: List[String]): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      val r = load(os.read(path), path, args)
      val n = Name(Some(path.baseName), name)
      r.procs.get(n) match
        case Some(d) if d.ins.isEmpty && d.outs.isEmpty && d.refs.isEmpty =>
          for line <- r.call(n) do
            println(line)
          true
        case _ =>
          error("runtime", s"there is no procedure $name without parameters")

  // runs each test of file on its own, all or the named ones, also after a failed one, printing whether it passed
  def tests(file: String, names: List[String], args: List[String]): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      val r = load(os.read(path), path, args)
      val selected =
        for
          case c @ Cmd.Test(n, _, _) <- blocks(r)
          if names.isEmpty || names.contains(n)
        yield c
      val all = for case Cmd.Test(n, _, _) <- blocks(r) yield n
      for n <- names do
        if !all.contains(n) then
          error("runtime", s"there is no test $n")
      val results =
        for case c @ Cmd.Test(name, _, _) <- selected
        yield
          try
            for line <- r.output(c) do
              println(line)
            println(s"test $name passed")
            true
          catch
            case e: Error =>
              println(s"test $name failed: ${e.getMessage}")
              false
      results.forall(identity)

  // only checks file
  def check(file: String): Boolean =
    attempt(file):
      val path = os.Path(file, os.pwd)
      load(os.read(path), path)
      true

  // translates file to C, whose main calls the procedure entry, or runs all tests for the entry test,
  // by the compiler of self/ulang.u in the interpreter, which the property ulang.self locates
  def compile(file: String, entry: String, target: String): Boolean =
    attempt(file):
      val dir = sys.props.get("ulang.self").map(os.Path(_, os.pwd)).getOrElse(error("runtime", "the property ulang.self does not name the directory self/"))
      val driver = dir / "ulang.u"
      val args = List("compile", os.Path(file, os.pwd).toString, entry, os.Path(target, os.pwd).toString)
      runs(driver.toString, "main", args)

  val usage =
    """usage: ulang run <file> [<procedure>] [-- args]
      |       ulang test <file> [<name> ...] [-- args]
      |       ulang check <file>
      |       ulang compile <file> <entry> <target.c>""".stripMargin

  def main(args: Array[String]): Unit =
    val (command, rest) = args.toList.span(_ != "--")
    val arguments = rest.drop(1)
    val ok = deep:
      command match
        case List("run", file) =>
          runs(file, "main", arguments)
        case List("run", file, name) =>
          runs(file, name, arguments)
        case "test" :: file :: names =>
          tests(file, names, arguments)
        case List("check", file) =>
          check(file)
        case List("compile", file, entry, target) =>
          compile(file, entry, target)
        case _ =>
          System.err.println(usage)
          false
    if !ok then
      sys.exit(1)
