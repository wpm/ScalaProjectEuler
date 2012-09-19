package projecteuler.net

import collection.mutable

object Collatz {
  def collatz(n: BigInt): Int = {
    val cache = mutable.Map[BigInt, Int](BigInt("1") -> 1)
    def collatzRec(n: BigInt, d: Int): Int = {
      if (cache.contains(n)) return d + cache(n)
      val r = n match {
        case m if (n % 2 == 0) => collatzRec(n / 2, d + 1)
        case _ => collatzRec(3 * n + 1, d + 1)
      }
      cache += n -> r
      r
    }
    collatzRec(n, 0)
  }
}
