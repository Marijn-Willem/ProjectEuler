object Problem1 extends ProblemSolver {
  override protected def getSolution: String = ((1000/3) + (1000/5) - (1000/15)).toString

  @main def print1(): Unit = print()
}
