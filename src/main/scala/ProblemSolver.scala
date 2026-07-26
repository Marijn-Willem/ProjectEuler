abstract class ProblemSolver {
  protected def getSolution: String

  private val startTime = System.currentTimeMillis()
  private val solution = getSolution
  private val duration = System.currentTimeMillis() - startTime

  protected def print(): Unit = println(s"$solution, found in $duration ms")
}
