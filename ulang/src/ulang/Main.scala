package ulang

object Main:
  // the values of evals and the failed tests
  def run(text: String): List[String] =
    new Run(Check.program(Parser.parseProgram(text))).run()

  def failed(line: String): Boolean =
    line.startsWith("test failed")

  // prints the output of file and whether it succeeded
  def report(file: String): Boolean =
    try
      val lines = run(os.read(os.Path(file, os.pwd)))
      for line <- lines do println(line)
      !lines.exists(failed)
    catch
      case e: Error =>
        System.err.println(s"$file: ${e.getMessage}")
        false

  def main(args: Array[String]): Unit =
    val results = args.toList.map(report)
    if results.contains(false) then sys.exit(1)
