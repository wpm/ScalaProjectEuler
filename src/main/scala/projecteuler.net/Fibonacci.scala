package projecteuler.net

object Fibonacci {
  val fibonacci: Stream[Int] = 0 #:: 1 #:: fibonacci.zip(fibonacci.tail).map(x => x._1 + x._2)
}
