import scala.annotation.tailrec

object Util {
  type Fractional = (Long, Long)

  def getPrimes(n: Int): Seq[Int] = {
    val sieveArr = Array.fill(n+1)(true)
    sieveArr(0) = false
    sieveArr(1) = false

    val maxI = Math.sqrt(n)

    var i = 2
    var j = 0

    while (i <= maxI) {
      if (sieveArr(i)) {
        j = i*i

        while (j <= n) {
          sieveArr(j) = false
          j += i
        }
      }

      i += 1
    }

    sieveArr.indices.filter(sieveArr)
  }

  def add(x: Fractional, y: Fractional): Fractional = {
    val num = (x._1*y._2) + (x._2*y._1)
    val den = x._2*y._2

    val gc = if (num == 0) den else gcd(num, den)

    (num/gc, den/gc)
  }

  @tailrec
  private def gcd(x: Long, y: Long): Long = {
    val xAbs = Math.abs(x)
    val yAbs = Math.abs(y)

    val mn = Math.min(xAbs, yAbs)
    val mx = Math.max(xAbs, yAbs)
    val md = mx % mn

    if (md == 0) mn else gcd(md, mn)
  }
}
