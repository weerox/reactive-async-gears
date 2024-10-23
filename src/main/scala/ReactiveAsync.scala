package rasync

import gears.async.default.given
import gears.async.{Async, Future}

object ReactiveAsync:
  def handler[V, T](body: Handler[V] ?=> T)(using lattice: Lattice[V]): T =
    val handler = Handler[V](lattice)
    val result = body(using handler)
    Async.blocking:
      val (cells, inits) = handler.initializers.toList.unzip
      val results = inits.map { init =>
        Future:
          init()
      }.awaitAll
      cells
        .zip(results)
        .map((cell, result) =>
          result match
            case Update(value)         => cell.update(value)
            case Complete(None)        => cell.complete()
            case Complete(Some(value)) => cell.update(value); cell.complete()
            case Nothing               =>
        )
    result

  def cell[V](using handler: Handler[V]): Cell[V] =
    val cell = CellUpdater[V]()
    handler.cells += cell
    cell

  def cell[V](init: () => Async ?=> Outcome[V])(using
      handler: Handler[V]
  ): Cell[V] =
    val cell = CellUpdater[V]()
    handler.cells += cell
    // For some reason, Scala doesn't like it if the tuple is used directly
    // in the assignment expression.
    val tmp = (cell, init)
    handler.initializers += tmp
    cell
