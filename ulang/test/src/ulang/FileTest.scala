package ulang

import utest.*

// runs every tests/**/*.u and examples/**/*.u, which must succeed including all assertions,
// or fail with the error stated in its first line, e.g. "// type error: unknown variable x"
object FileTest extends TestSuite:
  val roots = List("tests", "examples").map(os.Path(sys.env("MILL_WORKSPACE_ROOT")) / _)

  val error = "// ((parse|type|runtime) error: .*)".r

  def files: List[os.Path] =
    roots.flatMap(os.walk(_)).filter(_.ext == "u").sorted.toList

  // the problem with file, if any
  def problem(file: os.Path): Option[String] =
    val text = os.read(file)
    val expected = text.linesIterator.nextOption().collect { case error(msg, _) =>
      msg
    }
    try
      Main.run(text, file)
      expected.map(msg => s"$file: expected $msg")
    catch
      case e: Error if expected.contains(e.getMessage) =>
        None
      case e: Error =>
        Some(s"$file: expected ${expected.getOrElse("success")}, got ${e.getMessage}")

  val tests = Tests:
    test("files"):
      val problems = files.flatMap(problem)
      assert(files.nonEmpty, problems.isEmpty)
