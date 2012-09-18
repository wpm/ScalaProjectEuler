package projecteuler.net

import org.scalatest.FlatSpec

class FibonacciSpec extends FlatSpec {
  "The begining of the Fibonacci sequence" should "be 0,1,1,2,3,5,8,13,21" in {
    expect(List(0, 1, 1, 2, 3, 5, 8, 13, 21)) {
      Fibonacci.fibonacciSeries take 9
    }
  }

  it should "be 0,1,1,2,3,5,8,13,21 with the Dijkstra algorithm" in {
    expect(List(0, 1, 1, 2, 3, 5, 8, 13, 21)) {
      (0 to 8).map(Fibonacci.fibonacci(_))
    }
  }
}
