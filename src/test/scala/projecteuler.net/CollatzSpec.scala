package projecteuler.net

import org.scalatest.FlatSpec

class CollatzSpec extends FlatSpec {
  "Collatz(1)" should "be 1" in {
    expect(Nil) {
      Collatz.collatz(1)
    }
  }
  "Collatz(2)" should "be 2" in {
    expect(Seq(2)) {
      Collatz.collatz(2)
    }
  }
  "Collatz(3)" should "be 3, 10, 5, 16, 8, 4, 2" in {
    expect(Seq(3, 10, 5, 16, 8, 4, 2)) {
      Collatz.collatz(3)
    }
  }
  "Collatz(4)" should "be 4, 2" in {
    expect(Seq(4, 2)) {
      Collatz.collatz(4)
    }
  }
  "Collatz(13)" should "be 13, 40, 20, 10, 5, 16, 8, 4, 2" in {
    expect(Seq(13, 40, 20, 10, 5, 16, 8, 4, 2)) {
      Collatz.collatz(13)
    }
  }
}
