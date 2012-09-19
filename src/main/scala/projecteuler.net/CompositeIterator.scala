package projecteuler.net

/**
 * Iterator over the multiples of a prime number, starting at the square of that number
 * @param basePrime the base prime
 */
class CompositeIterator(basePrime: Int) extends BufferedIterator[Int] {
  private val prime = basePrime
  var composite = prime * prime

  def hasNext = true

  def head = composite

  def next() = {
    val n = composite
    composite += prime
    n
  }

  override def toString() = prime + ":" +
    (0 to 2).map(composite + _ * prime).map(_.toString).reduceLeft(_ + "," + _) + "..."
}

object CompositeIterator {
  def apply(basePrime: Int) = new CompositeIterator(basePrime)
}
