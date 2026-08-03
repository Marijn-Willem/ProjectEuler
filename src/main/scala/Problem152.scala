import Util.*

import scala.annotation.tailrec
import scala.language.implicitConversions

object Problem152 extends ProblemSolver {
  private lazy val limit = 80
  private lazy val primes = getPrimes(limit).tail
  private lazy val allowedNumbers = Array.fill(limit+1)(true)
  private lazy val numberToPrimeFactors: Seq[Seq[Int]] = Seq.empty +: Seq.empty +: Seq.empty +: Range(3, limit+1).map { n =>
    val primesToSqrt = primes.takeWhile(_ <= (n/2)).filter(p => (n%p) == 0)

    if (primesToSqrt.isEmpty) Seq(n) else primesToSqrt
  }
  private lazy val primeGroupMap: Map[Int, Seq[Seq[Int]]] =
    primes.reverse.map { p =>
      val groups = getGroupsForPrime(p)
      updateAllowedNumbers(groups, p)

      p -> groups
    }.toMap
  private lazy val powerOfTwoFractions: Seq[Fractional] = {
    @tailrec
    def f(cur: Seq[Fractional], pow2: Int): Seq[Fractional] =
      if (pow2 > limit)
        cur.tail
      else
        f(cur ++ cur.map(add(_, pow2)), 2*pow2)

    f(Seq((0, 1)), 2)
  }
  private lazy val validGroupCount: Int = {
    def isGroupValid(group: Seq[Int]): Boolean = {
      val fractionalSum = getFractionalSum(group)
      powerOfTwoFractions.exists(add(fractionalSum, _) == (1, 2))
    }

    def isPrimeGroupValid(group: Seq[Int], prime: Int, primeGroup: Seq[Int]): Boolean =
      group.filter(n => (n%prime) == 0).forall(primeGroup.contains) &&
        primeGroup.forall(n => group.contains(n) || !numberToPrimeFactors(n).exists(_ > prime))

    def getMandatoryPrimesFromPrimeGroup(prime: Int, primeGroup: Seq[Int]): Seq[Int] =
      primeGroup.flatMap(n => numberToPrimeFactors(n).takeWhile(_ < prime))

    def f(group: Seq[Int], mandatoryPrimes: Seq[Int], primeIndX: Int): Int = {
      if (primeIndX == -1)
        if (isGroupValid(group))
          1
        else
          0
      else {
        val prime = primes(primeIndX)

        val nonMandatoryPrimeSum = if (!mandatoryPrimes.contains(prime)) f(group, mandatoryPrimes, primeIndX-1) else 0

        primeGroupMap(prime).filter(isPrimeGroupValid(group, prime, _)).map { pg =>
          val mergedGroup = (group++pg).distinct
          val newMandatoryPrimes = (mandatoryPrimes ++ getMandatoryPrimesFromPrimeGroup(prime, pg)).distinct

          f(mergedGroup, newMandatoryPrimes, primeIndX-1)
        }.sum + nonMandatoryPrimeSum
      }
    }

    f(Seq.empty, Seq.empty, primes.size-1)
  }

  override protected def getSolution: String = validGroupCount.toString

  private given Conversion[Int, Fractional] with
    def apply(i: Int): Fractional = (1, i*i)

  private case class Group(nrs: Seq[Int], sum: Fractional)

  private def getGroupsForPrime(p: Int): Seq[Seq[Int]] = {
    def f(cur: Seq[Seq[Int]], toGo: Seq[Int]): Seq[Seq[Int]] =
      if (toGo.isEmpty)
        cur
      else if (allowedNumbers(toGo.head))
        f(cur, toGo.tail) ++ f(cur.map(_ :+ toGo.head), toGo.tail)
      else
        f(cur, toGo.tail)

    val multiplesP = Range(1, (limit/p)+1).map(p*_)
    f(Seq(Seq.empty), multiplesP).filter(isGroupValid(_, p))
  }

  private def isGroupValid(group: Seq[Int], p: Int): Boolean = {
    val fractionalSum = getFractionalSum(group)

    fractionalSum._1 != 0 && (fractionalSum._2%p) > 0
  }

  private def updateAllowedNumbers(groups: Seq[Seq[Int]], p: Int): Unit = {
    val arr = Array.fill((limit/p)+1)(false)
    groups.flatten.foreach(x => arr(x/p) = true)

    arr.indices.foreach(i => if (!arr(i)) allowedNumbers(i*p) = false)
  }

  private def getFractionalSum(group: Seq[Int]): Fractional =
    group.foldLeft((0,1):Fractional){ case (acc, x) => add(acc, x) }

  @main def print152(): Unit = print()
}
