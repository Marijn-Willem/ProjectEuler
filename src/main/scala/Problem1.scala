object Problem1 extends ProblemSolver {
  override protected def getSolution: String = Seq(3, 5, -15).map(1000/_).sum.toString

  @main override protected def print(): Unit = super.print()
}
