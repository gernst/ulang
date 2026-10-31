package ulang

import utest.*

// runs every tests/**/*.u, which must succeed with all tests passing,
// or fail with the error stated in its first line, e.g. "// type error: unknown variable x"
object FileTest extends TestSuite:
  val root = os.Path(sys.env("MILL_WORKSPACE_ROOT")) / "tests"

  val error = "// ((parse|type|runtime) error: .*)".r

  def files: List[os.Path] =
    os.walk(root).filter(_.ext == "u").sorted.toList

  // the problem with file, if any
  def problem(file: os.Path): Option[String] =
    val text = os.read(file)
    val expected = text.linesIterator.nextOption().collect { case error(msg, _) => msg }
    try
      val failures = Main.run(text).filter(Main.failed)
      if expected.nonEmpty
      then Some(s"$file: expected ${expected.get}")
      else Option.when(failures.nonEmpty)(s"$file: ${failures.mkString(", ")}")
    catch
      case e: Error if expected.contains(e.getMessage) => None
      case e: Error                                     => Some(s"$file: expected ${expected.getOrElse("success")}, got ${e.getMessage}")

  val tests = Tests:
    test("files"):
      val problems = files.flatMap(problem)
      assert(files.nonEmpty, problems.isEmpty)
