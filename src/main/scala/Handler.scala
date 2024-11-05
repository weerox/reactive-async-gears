package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

import scala.collection.mutable.{ ListBuffer, Map, MultiDict, Set }

import cell.{ Cell, CellUpdater }
import handler.DependencyHandler
import handler.InitializationHandler

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V] private[rasync] (val lattice: Lattice[V]):
  val cells: ListBuffer[CellUpdater[V]] = ListBuffer()

  val initializers: Map[
    CellUpdater[V],
    InitializationHandler[V]
  ] = Map()

  val dependencies: MultiDict[
    CellUpdater[V],
    DependencyHandler[V, ?, ?]
  ] = MultiDict()

  // Map a dependency to its dependent.
  // If cell A and cell B depends on cell C, then this will map C to A and B.
  // TODO This is currently populated by the `CellUpdater` in their `when` methods,
  // but I think it would be better to move that logic in here and abstract over
  // the types of `DependencyHandler`s. See the branch `only-run-neccessary`
  // for an idea of how to get the dependencies out of a handler.
  val dependents: MultiDict[
    Cell[V],
    CellUpdater[V]
  ] = MultiDict()

  // Using a tuple in the set should be fine, I think,
  // since one dependency handler only belongs to one cell.
  val scheduled: Set[(CellUpdater[V], DependencyHandler[V, ?, ?])] = Set()

  def initialize(): Unit =
    val (cells, handlers) = initializers.toSeq.unzip
    val results = Async.blocking:
      handlers.map { handler =>
        Future:
          handler.run()
      }.awaitAll
    cells
      .zip(results)
      .map { (cell, result) =>
        result match
          case Update(value)  => cell.update(value)
          case Complete(None) => cell.complete()
          case Complete(Some(value)) =>
            cell.update(value)
            cell.complete()
          case Nothing =>
      }

  def run(): Unit =
    scheduled.addAll(dependencies)

    while !scheduled.isEmpty do
      println(s"${dependencies.size}, ${scheduled.size}")

      // Run all the scheduled dependencies.
      val (dependents, handlers) = scheduled.toSeq.unzip

      val results = Async.blocking:
        handlers.map(handler =>
          Future:
            handler.run()
        ).awaitAll

      scheduled.clear()

      def schedule(cell: CellUpdater[V]) =
        // Get all cells for which `cell` is a dependency and schedule their handlers.
        val dependents = this.dependents.get(cell)
        scheduled.addAll(dependents.flatMap(dependent =>
          dependencies.get(dependent).map(handler => (dependent, handler))
        ))

      dependents.zip(results).map { (dependent, result) =>
        result match
          case Update(value) =>
            dependent.update(value)
            schedule(dependent)
          case Complete(None) =>
            dependent.complete()
            // We schedule relevant handlers when we complete a cell even if the value is not updated,
            // in case the handler won't act on any information until it receives a `Complete` cell.
            schedule(dependent)
          case Complete(Some(value)) =>
            dependent.update(value)
            dependent.complete()
            schedule(dependent)
          case Nothing =>
      }
