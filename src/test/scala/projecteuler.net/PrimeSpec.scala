package projecteuler.net

import org.scalatest.FlatSpec

class PrimeSpec extends FlatSpec {
  "The prime number sequence" should "start with 2,3,5,7,11" in {
    expect(List(2, 3, 5, 7, 11)) {
      (Prime.primes take 5).toList
    }
  }
}
