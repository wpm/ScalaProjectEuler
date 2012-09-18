package projecteuler.net

import org.scalatest.FlatSpec

class Problems extends FlatSpec {
  "1. The sum of all multiples of 3 and 4 less than 1000" should "be 234168" in {
    expect(234168) {
      1 to 1000 filter (n => n % 3 == 0 || n % 5 == 0) sum
    }
  }

  "2. The sum of the even Fibonacci numbers below four million" should "be 4613732" in {
    expect(4613732) {
      Fibonacci.fibonacciSeries takeWhile (_ <= 4000000) filter (_ % 2 == 0) sum
    }
  }

  "3. The largest prime factor of 600851475143" should "be 6271" in {
    expect(6857) {
      Primes.factors(BigInt.apply("600851475143")).max
    }
  }

  "6. Difference of sqaured sum and sum of sqaures for 1...100" should "be 25164150" in {
    expect(25164150) {
      def square(x: Int) = x * x
      def diff(n: Int) = square((1 to n).sum) - (1 to n).map(square(_)).sum
      diff(100)
    }
  }

  "7. The 10,001st prime number" should "be 104743" in {
    expect(104743) {
      val primes = new Primes()
      primes.drop(10000)
      primes.next()
    }
  }

  "10. The sum of primes below two million" should "be 1159902918" in {
    expect(1159902918) {
      val primes = new Primes()
      primes.takeWhile(_ < 2000000).sum
    }
  }

  "25. The first Fibonacci number with 1000 digits" should "be number 4782 in the series" in {
    expect(4782) {
      // Use the log of Binet's expression to know to start around 4000.
      val start = 4000
      def ints(n: Int): Stream[Int] = n #:: ints(n + 1)
      ints(start).map(Fibonacci.fibonacci(_).toString().length).indexOf(1000) + start
    }
  }
}
