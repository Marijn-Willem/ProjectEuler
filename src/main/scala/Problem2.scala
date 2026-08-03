object Problem2 extends ProblemSolver {
  override protected def getSolution: String = {
    var x = 1
    var y = 2
    var s = 0L

    while (y < 4e6) {
      s += y.toLong
      val xNew = 2*y+x
      y = 3*y+2*x
      x = xNew
    }

    s.toString
  }

  @main def print2(): Unit = print()
}
