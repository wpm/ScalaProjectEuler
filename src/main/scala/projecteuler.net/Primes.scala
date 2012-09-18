package projecteuler.net

import collection.mutable
import annotation.tailrec

/**
 * Iterator over the prime numbers.
 */
class Primes extends Iterator[Int] {

  /**
   * Iterator over all the composite multiplies of a given prime p starting from p squared.
   * @param init initial prime
   */
  class CompositeIterator(init: Int) extends Iterator[Int] {
    private val prime = init
    var composite = prime * prime

    def hasNext = true

    def next() = {
      composite += prime
      composite
    }

    override def toString() = (0 to 3).map(composite + _ * prime).map(_.toString).reduceLeft(_ + "," + _) + "..."
  }

  // TODO Implement sieve of Eratosthenes with priority queue instead of multimap.
  /**
   * Sieve of Eratosthenes as described in "The Genuine Sieve of Eratosthenes" by Melissa E. O'Neill.
   *
   * Each time a prime p is discovered an iterator over its multiples starting at p*p is added to a table, indexed by
   * the next multiple. If p+1 does not appear in this table of multiples, it is another prime. Otherwise, it is a
   * composite, in which case all the corresponding iterators are advanced.
   */
  class SieveOfEratosthenes
    extends mutable.HashMap[Int, mutable.Set[CompositeIterator]] with mutable.MultiMap[Int, CompositeIterator] {
    def addCompositeIterator(prime: Int) {
      addBinding(prime * prime, new CompositeIterator(prime))
    }

    def advanceIterators(composite: Int) {
      for (i <- remove(composite).get) {
        addBinding(i.next(), i)
      }
    }
  }

  private val sieve: SieveOfEratosthenes = new SieveOfEratosthenes()
  private var prime: Int = 1

  def hasNext = true

  def next(): Int = {
    prime += 1
    while (sieve.contains(prime)) {
      sieve.advanceIterators(prime)
      prime += 1
    }
    sieve.addCompositeIterator(prime)
    prime
  }
}

object Primes {
  /**
   * Prime factorization of a number
   *
   * @param n number to factorize
   * @return prime factors of n
   */
  def factors(n: BigInt): List[BigInt] = {
    @tailrec
    def factorsRecurse(fs: List[BigInt], n: BigInt): List[BigInt] = {
      val primes = new Primes
      primes takeWhile (_.toDouble <= math.sqrt(n.toDouble)) find (n % _ == 0) match {
        case Some(factor) => factorsRecurse(factor :: fs, n / factor)
        case None => n :: fs
      }
    }
    factorsRecurse(Nil, n).reverse
  }
}