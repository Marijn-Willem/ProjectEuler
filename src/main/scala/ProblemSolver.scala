abstract class ProblemSolver {
  protected def getSolution: String

  protected def print(): Unit = {
    val startTime = System.currentTimeMillis()
    val solution = getSolution
    val duration = System.currentTimeMillis() - startTime

    println(s"$solution, found in $duration ms")
  }
}
