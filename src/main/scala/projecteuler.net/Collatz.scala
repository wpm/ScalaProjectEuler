package projecteuler.net

import collection.mutable

/**
 * Algorithms to investigate the Collatz hypothesis of Hailstone chains.
 */
class Collatz {
  /**
   * Hailstone chain
   */
  val hailstone = Stream.iterate(_: BigInt)(n => if (n % 2 == 0) n / 2 else 3 * n + 1).takeWhile(_ > 1) :+ BigInt("1")
  /**
   * Map from a start value to the length of the corresponding Hailstone chain
   */
  private val cachedLength = mutable.Map[BigInt, Int](BigInt("1") -> 1)

  /**
   * The length of a Hailstone chain
   *
   * This has the side effect of updating the length cache in this instance.
   *
   * @param start the starting value
   * @return length of the chain starting at the specified value
   */
  def chainLength(start: BigInt): Int = {
    // Run the hailstone chain from the starting value until the first cached length.
    val chain = hailstone(start)
    val newChain = chain.takeWhile(n => !cachedLength.contains(n))
    // Add the new chain elements the the length cache.
    val l = newChain.length
    val d = cachedLength(chain(l))
    for ((n, i) <- newChain.take(newChain.length).zipWithIndex) cachedLength(n) = d + l - i
    l + d
  }

  /**
   * The longest chain in a range of starting values from one to a specified maximum
   *
   * All the chains share the same length cache.
   *
   * @param maxStart the largest starting value
   * @return (chain length, starting value) for the longest chain
   */
  def maxChain(maxStart: Int) = (1 to maxStart).map(n => ((chainLength(n), n))).max
}
