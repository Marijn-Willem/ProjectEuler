object Problem296 extends ProblemSolver {
  private lazy val limit = 100000

  override protected def getSolution: String = (getCount(1, 1) + iterate(0, 1, 1, 1)).toString

  private def iterate(ln: Int, ld: Int, rn: Int, rd: Int): Long = {
    val mn = ln+rn
    val md = ld+rd

    if (tupleFilter(mn, md))
      getCount(mn, md) + iterate(ln, ld, mn, md) + iterate(mn, md, rn, rd)
    else
      0
  }

  private def tupleFilter(a: Int, b: Int): Boolean = 223*a > b && 3*a+6*b <= limit

  private def getCount(a: Int, b: Int): Long = {
    val ab = a+b
    var count = 0L
    var d = if (b==1) 2 else 3
    var abSum = d*ab

    while (abSum + (d*b) <= limit) {
      val validC = (d*a) / ab
      val validCAboveLimit = Math.max((2*abSum)-limit-1, 0) / ab
      count += (validC-validCAboveLimit).toLong
      d += 1
      abSum += ab
    }

    count
  }

  @main def print296(): Unit = print()
}
