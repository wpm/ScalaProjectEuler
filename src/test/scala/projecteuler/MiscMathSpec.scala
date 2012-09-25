package projecteuler

import org.scalatest.FlatSpec
import MiscMath._

class MiscMathSpec extends FlatSpec {
  "40!" should "be 815915283247897734345611269596115894272000000000" in {
    expect(BigInt("815915283247897734345611269596115894272000000000")) {
      factorial(40)
    }
  }

  "The 5th row of Pascal's triangle" should "be 1 5 10 10 5 1" in {
    expect(List(1, 5, 10, 10, 5, 1)) {
      (0 to 5).map(choose(5, _))
    }
  }

  "The big integer choose(100,7) = 16007560800" should "be the same calculated recursively or with factorials" in {
    expect(factorial(100) / (factorial(7) * factorial(100 - 7))) {
      choose(100, 7)
    }
  }
}
