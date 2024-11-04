package rasync
package lattice

class NaturalNumberLattice extends Lattice[Int]:
  override val bottom: Int                   = 0
  override def join(x: Int, y: Int): Int     = x.max(y)
  override def lteq(x: Int, y: Int): Boolean = x <= y

given Lattice[Int] = NaturalNumberLattice()
