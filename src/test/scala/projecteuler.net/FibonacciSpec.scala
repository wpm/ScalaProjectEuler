package projecteuler.net

import org.scalatest.FlatSpec

class FibonacciSpec extends FlatSpec {
  "The begining of the Fibonacci sequence" should "be 0,1,1,2,3,5,8,13,21" in {
    expect(List(0, 1, 1, 2, 3, 5, 8, 13, 21)) {
      Fibonacci.fibonacci take 9
    }
  }
}