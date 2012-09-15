package projecteuler.net

import org.scalatest.FlatSpec

class PrimeSpec extends FlatSpec {
  "The prime number sequence" should "start with 2,3,5,7,11" in {
    expect(List(2, 3, 5, 7, 11)) {
      Prime.primes.take(5)
    }
  }

  it should "have 13 as its sixth element" in {
    expect(13) {
      Prime.primes(5)
    }
  }

  "The factors of 2" should "be 2" in {
    expect(List(2)) {
      Prime.factors(2)
    }
  }

  "The factors of 17" should "be 17" in {
    expect(List(17)) {
      Prime.factors(17)
    }
  }

  "The factors of 220" should "be 2 2 5 11" in {
    expect(List(2, 2, 5, 11)) {
      Prime.factors(220)
    }
  }

  "The factors of 63525" should "be 3 5 5 7 11 11" in {
    expect(List(3, 5, 5, 7, 11, 11)) {
      Prime.factors(63525)
    }
  }

  "The factors of 600851475143" should "be 3 277 1153 6271" in {
    expect(List(3, 277, 1153, 6271)) {
      Prime.factors(BigInt.apply("6008514753"))
    }
  }
}
