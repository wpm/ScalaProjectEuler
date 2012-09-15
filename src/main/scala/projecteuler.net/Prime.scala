package projecteuler.net

object Prime {
  /**
   * The prime numbers
   */
  val primes: Stream[Int] = {
    def integers(n: Int): Stream[Int] = n #:: integers(n + 1)
    def eratosthenes(ns: Stream[Int]): Stream[Int] = ns.head #:: eratosthenes((ns tail) filter (_ % ns.head != 0))
    eratosthenes(integers(2))
  }
}
