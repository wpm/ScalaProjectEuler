package projecteuler

object MiscMath {
  /**
   * Binomial coefficient
   *
   * This uses the recurrence C(n+1, k+1) = (n+1)/(k+1) C(n,k).
   *
   * @param n number of items
   * @param k number of items to choose
   * @return n choose k
   */
  def c(n: Int, k: Int): Int = (n, k) match {
    case (_, 0) => 1
    case (0, _) => 0
    // Must multiply before dividing in order to get an integer, so can't do tail recursion with an accumulator.
    case _ => n * c(n - 1, k - 1) / k
  }
}
