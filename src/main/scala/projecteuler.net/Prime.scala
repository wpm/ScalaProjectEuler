package projecteuler.net

import math.sqrt

object Prime {
  /**
   * The prime numbers
   */
  val primes: Stream[BigInt] = {
    def integers(n: BigInt): Stream[BigInt] = n #:: integers(n + 1)
    def eratosthenes(ns: Stream[BigInt]): Stream[BigInt] = ns.head #:: eratosthenes((ns tail) filter (_ % ns.head != 0))
    eratosthenes(integers(2))
  }

  /**
   * Prime factorization of a number
   *
   * @param n number to factorize
   * @return prime factors of n
   */
  def factors(n: BigInt): Stream[BigInt] = {
    primes takeWhile (_.toDouble <= sqrt(n.toDouble)) find (n % _ == 0) match {
      case Some(factor) => factor #:: factors(n / factor)
      case None => n #:: Stream.empty
    }
  }
}
