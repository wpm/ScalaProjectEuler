package projecteuler

import collection.mutable

/**
 * Algorithms to calculate the Fibonacci series
 *
 * This contains an algorithm to produce the entire series as a stream and an implementation of Dijkstra's recursive
 * method for finding a single term of the series.
 */
object Fibonacci {
  /**
   * The Fibonacci series
   */
  lazy val fibonacciSeries: Stream[BigInt] = {
    def fibonacciGenerator(x: BigInt, y: BigInt): Stream[BigInt] = x #:: fibonacciGenerator(y, x + y)
    fibonacciGenerator(0, 1)
  }

  /**
   * The nth Fibonacci number
   *
   * This uses Dijkstra's recursive algorithm.
   *
   * @param n series index
   * @return the nth Fibonacci number
   */
  def fibonacci(n: Int): BigInt = {
    var memo = mutable.Map[Int, BigInt]()

    def df(n: Int): BigInt = {
      def square(x: BigInt) = x * x

      if (memo.contains(n)) return memo(n)

      val result: BigInt = n match {
        case 0 => 0
        case 1 => 1
        // F(2n-1) = F(n-1)^2 + F(n)^2
        case m if (n % 2 == 1) => {
          val q = (n + 1) / 2
          square(df(q - 1)) + square(df(q))
        }
        // F(2n) = ( 2 F(n-1) + F(n) ) F(n)
        case _ => {
          val q = n / 2
          (2 * df(q - 1) + df(q)) * df(q)
        }
      }
      memo += n -> result
      result
    }
    df(n)
  }
}
