package projecteuler.net

import org.scalatest.FlatSpec

class PrimesSpec extends FlatSpec {
  def fixture = new {
    val primes = SieveOfEratosthenes()
  }

  "The prime number sequence" should "start with 2,3,5,7,11" in {
    expect(List(2, 3, 5, 7, 11)) {
      val f = fixture
      for (_ <- 1 to 5) yield f.primes.next()
    }
  }

  it should "have 13 as its sixth element" in {
    expect(13) {
      val f = fixture
      f.primes.drop(5)
      f.primes.next()
    }
  }

  "The factors of 2" should "be 2" in {
    expect(List(2)) {
      Primes.factors(2)
    }
  }

  "The factors of 17" should "be 17" in {
    expect(List(17)) {
      Primes.factors(17)
    }
  }

  "The factors of 220" should "be 2 2 5 11" in {
    expect(List(2, 2, 5, 11)) {
      Primes.factors(220)
    }
  }

  "The factors of 63525" should "be 3 5 5 7 11 11" in {
    expect(List(3, 5, 5, 7, 11, 11)) {
      Primes.factors(63525)
    }
  }

  "The factors of 600851475143" should "be 71 839 1471 6857" in {
    expect(List(71, 839, 1471, 6857)) {
      Primes.factors(BigInt.apply("600851475143"))
    }
  }
}
