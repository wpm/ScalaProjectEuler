package projecteuler.net

import org.scalatest.FlatSpec

class FibonacciSpec extends FlatSpec {
  "The beginng of the Fibonacci sequence" should "be 0,1,1,2,3,5,8,13,21" in {
    expect(List(0,1,1,2,3,5,8,13,21)) {
      Fibonacci.fibonacci take 9
    }
  }

  "2. The sum of the even Fibonacci numbers below 24" should "be 2+8=10" in {
    expect(10) {
      Fibonacci.fibonacci takeWhile(_ <= 24) filter (_ % 2 == 0) sum
    }
  }
}
