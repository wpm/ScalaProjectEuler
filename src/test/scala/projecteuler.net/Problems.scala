package projecteuler.net

import org.scalatest.FlatSpec

class Problems extends FlatSpec {
  "1. The sum of all multiples of 3 and 4 less than 1000" should "be 234168" in {
    expect(234168) {
      1 to 1000 filter (n=> n%3 == 0 || n %5 ==0) sum
    }
  }

  "2. The sum of the even Fibonacci numbers below four million" should "be 4613732" in {
    expect(4613732) {
      Fibonacci.fibonacci takeWhile(_ <= 4000000) filter (_ % 2 == 0) sum
    }
  }

  "3. The largest prime factor of 600851475143" should "be 6271" in {
    expect(6271) {
      NumberTheory.factors(BigInt.apply("6008514753")).max
    }
  }
}