package projecteuler.net

import collection.mutable

object Fibonacci {
  // Fibonacci.fibonacci(1000000) runs out of memory
  val fibonacci: Stream[BigInt] = 0 #:: 1 #:: fibonacci.zip(fibonacci.tail).map(x => x._1 + x._2)

  /**
   * Find the nth Fibonacci number using Dijkstra's recursive method
   * @param n series index
   * @return the nth Fibonacci number
   */
  def dijkstraFibonacci(n: Int): BigInt = {
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
