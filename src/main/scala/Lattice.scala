package rasync

trait Lattice[T]:
	/** Returns `true` iff `x` precedes `y` */
	def lteq(x: T, y: T): Boolean

	/** Computes the join of the two values, also known as the least upper bound */
	def join(x: T, y: T): T

	val bottom: T