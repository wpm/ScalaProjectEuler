package projecteuler

import annotation.tailrec

object MiscMath {
  def factorial(n: Int): BigInt = ((1 to n) :\ BigInt(1))(_ * _)

  /**
   * Binomial coefficient
   *
   * This uses the recurrence C(n+1, k+1) = (n+1)/(k+1) C(n,k).
   *
   * @param n number of items
   * @param k number of items to choose
   * @return n choose k
   */
  def choose(n: Int, k: Int): BigInt = {
    @tailrec
    def chooseRec(n: Int, k: Int, acc: BigDecimal): BigInt =
      (n, k) match {
        case (_, 0) => acc.toBigInt()
        case (0, _) => 0
        case _ => chooseRec(n - 1, k - 1, acc * n / k)
      }
    chooseRec(n, k, 1)
  }
}

