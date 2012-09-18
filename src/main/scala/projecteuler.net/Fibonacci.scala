package projecteuler.net

import collection.mutable

object Fibonacci {
  // Fibonacci.fibonacciSeries(1000000) runs out of memory

  /**
   * The entire Fibonacci series
   */
  val fibonacciSeries: Stream[BigInt] = 0 #:: 1 #:: fibonacciSeries.zip(fibonacciSeries.tail).map(x => x._1 + x._2)

  /**
   * The nth Fibonacci number
   *
   * This uses Dijkstra's recursive method, which is more efficient for large n.
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
