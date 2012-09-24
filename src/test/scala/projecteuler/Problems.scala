package projecteuler

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
      SieveOfEratosthenes.factors(BigInt.apply("600851475143")).max
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
      val primes = SieveOfEratosthenes()
      primes.drop(10000)
      primes.next()
    }
  }

  "9. a*b*c where a+b+c=1000 and a,b,c is a Pythagorean triple" should "be 31875000 (a=200,b=375,c=425)" in {
    expect(31875000) {
      (for (a <- 1 to 334;
            b <- a + 1 to 500;
            c <- b + 1 to 500
            if ((a + b + c == 1000) && (a * a + b * b == c * c)))
      yield ((a * b * c))).head
    }
  }

  "10. The sum of primes below two million" should "be 1179908154" in {
    expect(1179908154) {
      val primes = SieveOfEratosthenes()
      primes.takeWhile(_ < 2000000).sum
    }
  }

  // TODO 500 divisors takes a long time.
  "12. The first triangle number to have more than 5 divisors" should "be 28" in {
    expect(28) {
      def triangle(n: Int) = n * (n + 1) / 2
      def divisors(n: Int) = (1 to n).count(n % _ == 0)
      def ints(n: Int): Stream[Int] = n #:: ints(n + 1)
      ints(0).map(triangle(_)).find(divisors(_) > 5).get
    }
  }

  "14. The longest Hailstone chain beneath a million" should "start on 837799 and be 525 numbers long" in {
    expect(((525, 837799))) {
      val c = new Collatz()
      c.maxChain(1000000)
    }
  }

  "20. The sum of the digits in 100!" should "be 648" in {
    expect(648) {
      implicit def pimp(n: Int) = new {
        def !(): BigInt = ((1 to n) :\ BigInt(1))(_ * _)
      }
      (100!).toString().map(_.toString.toInt).sum
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

  "53. The number of C(n,k)>1000000 with n <= 100" should "be 2200" in {
    expect(2200) {
      (for (n <- 1 to 100; k <- 0 to n) yield MiscMath.c(n, k)).filter(_ > 1000000).length
    }
  }
}
