package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

import scala.collection.mutable.Map

import cell.CellUpdater
import handler.InitializationHandler

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V] private[rasync] (val lattice: Lattice[V]):
  var cells: List[CellUpdater[V]] = List()

  val initializers: Map[
    CellUpdater[V],
    InitializationHandler[V]
  ] = Map()

  def initialize(): Unit =
    val (cells, handlers) = initializers.toSeq.unzip
    val results = Async.blocking:
      handlers.map { handler =>
        Future:
          try
            Right(handler.run())
          catch
            case e => Left(e)
      }.awaitAll
    cells
      .zip(results)
      .map { (cell, result) =>
        result match
          case Left(e) => cell.fail(e)
          case Right(outcome) => outcome match
              case Update(value)  => cell.update(value)
              case Complete(None) => cell.complete()
              case Complete(Some(value)) =>
                cell.update(value)
                cell.complete()
              case Nothing =>
      }

  def run(): Unit =
    def dependencies = cells.flatMap(cell => cell.dependencies.map(dep => (cell, dep)))

    while
      !dependencies.isEmpty
    do
      val (dependents, handlers) = dependencies.unzip

      val results = Async.blocking:
        handlers.map(handler =>
          Future:
            try
              Right(handler.run())
            catch
              case e => Left(e)
        ).awaitAll

      dependents.zip(results).map { (dependent, result) =>
        result match
          case Left(e) => dependent.fail(e)
          case Right(outcome) => outcome match
              case Update(value)  => dependent.update(value)
              case Complete(None) => dependent.complete()
              case Complete(Some(value)) =>
                dependent.update(value)
                dependent.complete()
              case Nothing =>
      }
