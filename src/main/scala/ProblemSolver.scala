abstract class ProblemSolver {
  protected def getSolution: String

  private val startTime = System.currentTimeMillis()
  private lazy val solution = getSolution
  private lazy val duration = System.currentTimeMillis() - startTime

  protected def print(): Unit = println(s"$solution, found in $duration ms")
}
