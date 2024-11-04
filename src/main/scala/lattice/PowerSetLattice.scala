package rasync
package lattice

class PowerSetLattice[T] extends Lattice[Set[T]]:
  override val bottom: Set[T]                      = Set()
  override def join(x: Set[T], y: Set[T]): Set[T]  = x union y
  override def lteq(x: Set[T], y: Set[T]): Boolean = x subsetOf y

given [T]: Lattice[Set[T]] = PowerSetLattice[T]()
