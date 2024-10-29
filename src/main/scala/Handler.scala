package rasync

import gears.async.default.given
import gears.async.{Async, Future}
import scala.collection.mutable.ListBuffer

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V] private[rasync] (val lattice: Lattice[V]):
  val cells: ListBuffer[CellUpdater[V]] = ListBuffer()
  val initializers: ListBuffer[(CellUpdater[V], () => Async ?=> Outcome[V])] =
    ListBuffer()

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

  def run(): Unit =
    var whens: Seq[
      (CellUpdater[V], (Iterable[Cell[V]], Iterable[State[V]] => Outcome[V]))
    ] = Seq()

    while
      whens = cells.toList
        .flatMap(dependent =>
          dependent.dependencies.iterator.map((dependencies, code) =>
            (dependent, (dependencies, code))
          )
        )
      whens.size != 0
    do
      val (dependents, code) = whens.unzip

      val results = Async.blocking:
        code.map { (dependencies, code) =>
          Future:
            code(dependencies.map(cell => cell.state))
        }.awaitAll

      dependents.zip(results).map { (dependent, result) =>
        result match
          case Update(value)  => dependent.update(value)
          case Complete(None) => dependent.complete()
          case Complete(Some(value)) =>
            dependent.update(value); dependent.complete()
          case Nothing =>
      }
