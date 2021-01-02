package org.bobosergiusz.patternmatcher

// TODO: clean
object main {
  def main(args: Array[String]): Unit = {
    val filePath = args(0)

    val pipe = assemblePipeline(args)

    val linesFromFile = IO.ReadFile(filePath)

    def processLine(out: Output): IO[(Unit, Output)] = for {
        line <- linesFromFile
        unitOut <- line match {
          case None => IO.unit((), out)
          case Some(l) => {
            val (res, out2) = pipe(l)(out)
            for {
              _ <- IO.PrintLine(res)
              unitOut <- processLine(out2)
            } yield unitOut
          }
        }
      } yield unitOut

    val marker: Output = Marker("\u001B[31m", "\u001B[0m", 2, 2)

    val io = processLine(marker)

    io.run
  }
  def assemblePipeline(args: Array[String]): String => Output => (String, Output) = {
    val patternString = args(1)
    var pattern = Pattern.regex(patternString)
    s: String => o: Output => {
      val r = pattern.matches(s)
      o(s, r)
    }
  }
}
