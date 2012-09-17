package projecteuler.net

import org.scalatest.FlatSpec

class NumberTheorySpec extends FlatSpec {
  "The factors of 2" should "be 2" in {
    expect(List(2)) {
      NumberTheory.factors(2)
    }
  }

  "The factors of 17" should "be 17" in {
    expect(List(17)) {
      NumberTheory.factors(17)
    }
  }

  "The factors of 220" should "be 2 2 5 11" in {
    expect(List(2, 2, 5, 11)) {
      NumberTheory.factors(220)
    }
  }

  "The factors of 63525" should "be 3 5 5 7 11 11" in {
    expect(List(3, 5, 5, 7, 11, 11)) {
      NumberTheory.factors(63525)
    }
  }

  "The factors of 600851475143" should "be 71 839 1471 6857" in {
    expect(List(71, 839, 1471, 6857)) {
      NumberTheory.factors(BigInt.apply("600851475143"))
    }
  }
}
