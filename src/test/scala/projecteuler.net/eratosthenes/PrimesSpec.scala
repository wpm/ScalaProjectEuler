package projecteuler.net.eratosthenes

import org.scalatest.FlatSpec

class PrimesSpec extends FlatSpec {
  def fixture = new {
    val primes = new Primes
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
}
