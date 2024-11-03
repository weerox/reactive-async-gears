package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

import scala.collection.mutable.{ ListBuffer, Map, MultiDict }

import cell.CellUpdater
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
    while dependencies.size != 0 do
      val (dependents, handlers) = dependencies.toSeq.unzip

      val results = Async.blocking:
        handlers.map(handler =>
          Future:
            handler.run()
        ).awaitAll

      dependents.zip(results).map { (dependent, result) =>
        result match
          case Update(value)  => dependent.update(value)
          case Complete(None) => dependent.complete()
          case Complete(Some(value)) =>
            dependent.update(value)
            dependent.complete()
          case Nothing =>
      }
