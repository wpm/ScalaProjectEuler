package projecteuler.net

import collection.mutable

/**
 * Iterator over prime numbers that uses the sieve of Eratosthenes
 *
 * This is an implementation of the sieve of Eratosthenes as described in "The Genuine Sieve of Eratosthenes" by
 * Melissa E. O'Neill.
 *
 * Each time a prime p is discovered an iterator over its multiples starting at p*p is added to a priority queue
 * indexed by its next multiple. If p+1 does not appear at the head of the queue, it is another prime. Otherwise, it
 * is a composite, in which case all the corresponding iterators are advanced.
 */
class SieveOfEratosthenes extends BufferedIterator[Int] {

  /**
   * Iterators with the smallest next elements come first in the priority queue.
   */
  object CompositeIteratorOrdering extends Ordering[CompositeIterator] {
    def compare(a: CompositeIterator, b: CompositeIterator) = b.head.compare(a.head)
  }

  /**
   * Iterators over the multiples of all the primes that have been discovered so far.
   */
  private val composites = new mutable.PriorityQueue[CompositeIterator]()(CompositeIteratorOrdering)

  private var n: Int = 2
  markPrime(2)

  def hasNext = true

  def next() = {
    val prime = n
    n += 1
    while (isComposite(n)) {
      markComposite(n)
      n += 1
    }
    markPrime(n)
    prime
  }

  def head = n

  override def toString() = composites.toList.sortWith(_.head < _.head).mkString(", ")

  private def isComposite(n: Int) = composites.head.head == n

  private def markPrime(prime: Int) {
    composites.enqueue(new CompositeIterator(prime))
  }

  private def markComposite(composite: Int) {
    // Dequeue all the prime multiple iterators that have this composite as their next element.
    val its = composites.takeWhile(_.head == composite).toList
    for (_ <- 1 to its.length) composites.dequeue()
    // Advance the removed iterators and then add them back in.
    its.foreach(_.next())
    composites ++= its
  }
}

object SieveOfEratosthenes {
  def apply() = new SieveOfEratosthenes
}
