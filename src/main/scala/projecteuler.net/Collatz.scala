package projecteuler.net


object Collatz {
  val collatz = Stream.iterate(_: BigInt)(n => if (n % 2 == 0) n / 2 else 3 * n + 1).takeWhile(_ > 1)
}
