package projecteuler.net

/**
 * Various number theory functions.
 */
object NumberTheory {
  /**
   * Prime factorization of a number
   *
   * @param n number to factorize
   * @return prime factors of n
   */
  def factors(n: BigInt): Stream[BigInt] = {
    val primes = new Primes
    primes takeWhile (_.toDouble <= math.sqrt(n.toDouble)) find (n % _ == 0) match {
      case Some(factor) => factor #:: factors(n / factor)
      case None => n #:: Stream.empty
    }
  }
}
