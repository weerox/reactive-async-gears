package rasync

import gears.async.default.given
import gears.async.{Async, Future}
import scala.collection.mutable.ListBuffer

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V](val lattice: Lattice[V]):
  private[rasync] val cells: ListBuffer[CellUpdater[V]] = ListBuffer()
  private[rasync] val initializers
      : ListBuffer[(CellUpdater[V], () => Async ?=> Outcome[V])] = ListBuffer()

  def initialize(): Unit =
    val (cells, inits) = initializers.toList.unzip
    val results = Async.blocking:
      inits.map { init =>
        Future:
          init()
      }.awaitAll
    cells
      .zip(results)
      .map { (cell, result) =>
        result match
          case Update(value)         => cell.update(value)
          case Complete(None)        => cell.complete()
          case Complete(Some(value)) => cell.update(value); cell.complete()
          case Nothing               =>
      }
