import scala.collection.mutable

object Problem296 extends ProblemSolver {
  private lazy val limit = 100000

  override protected def getSolution: String = {
    var count = getCount(1, 1)
    val queue = mutable.Queue.empty[(Int, Int)]
    queue.enqueue((1, 2))

    while (queue.nonEmpty) {
      val (a, b) = queue.dequeue()

      count += getCount(a, b)

      val tplLeft = (a, a+b)
      val tplRight = (b, a+b)

      if (tupleFilter(tplLeft))
        queue.enqueue(tplLeft)

      if (tupleFilter(tplRight))
        queue.enqueue(tplRight)
    }

    count.toString
  }

  private def tupleFilter(tpl: (Int, Int)): Boolean = (3*tpl._1) + (6*tpl._2) <= limit

  private def getCount(a: Int, b: Int): Long = {
    var count = 0L
    var f = 2
    var abSum = f*(a+b)

    while (abSum + (f*b) <= limit) {
      val validC = (f*a) / (a+b)
      val validCAboveLimit = Math.max((2*abSum)-limit-1, 0) / (a+b)

      count += (validC-validCAboveLimit).toLong
      f += 1
      abSum += (a+b)
    }

    count
  }

  @main def print296(): Unit = print()
}
