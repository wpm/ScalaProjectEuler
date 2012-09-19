package projecteuler.net

import org.scalatest.FlatSpec

class CollatzSpec extends FlatSpec {
  "Collatz(1)" should "be 1" in {
    expect(1) {
      Collatz.collatz(1)
    }
  }
  "Collatz(2)" should "be 2" in {
    expect(2) {
      Collatz.collatz(2)
    }
  }
  "Collatz(3)" should "be 8" in {
    expect(8) {
      Collatz.collatz(3)
    }
  }
  "Collatz(4)" should "be 3" in {
    expect(3) {
      Collatz.collatz(4)
    }
  }
  "Collatz(13)" should "be 10" in {
    expect(10) {
      Collatz.collatz(13)
    }
  }
}
