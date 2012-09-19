package projecteuler.net

import org.scalatest.FlatSpec

class CollatzSpec extends FlatSpec {
  def fixture = new {
    val collatz = new Collatz
  }

  "Hailstone(1)" should "be 1" in {
    expect(Seq(1)) {
      fixture.collatz.hailstone(1)
    }
  }
  "Hailstone(2)" should "be 2, 1" in {
    expect(Seq(2, 1)) {
      fixture.collatz.hailstone(2)
    }
  }
  "Hailstone(3)" should "be 3, 10, 5, 16, 8, 4, 2, 1" in {
    expect(Seq(3, 10, 5, 16, 8, 4, 2, 1)) {
      fixture.collatz.hailstone(3)
    }
  }
  "Hailstone(4)" should "be 4, 2, 1" in {
    expect(Seq(4, 2, 1)) {
      fixture.collatz.hailstone(4)
    }
  }
  "Hailstone(13)" should "be 13, 40, 20, 10, 5, 16, 8, 4, 2, 1" in {
    expect(Seq(13, 40, 20, 10, 5, 16, 8, 4, 2, 1)) {
      fixture.collatz.hailstone(13)
    }
  }
}
