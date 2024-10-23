package rasync

import gears.async.Async
import scala.collection.mutable.ListBuffer

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
*/
class Handler[V](val lattice: Lattice[V]):
	private[rasync] val cells: ListBuffer[CellUpdater[V]] = ListBuffer()
	private[rasync] val initializers: ListBuffer[(CellUpdater[V], () => Async ?=> Outcome[V])] = ListBuffer()
