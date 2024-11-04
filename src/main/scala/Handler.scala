package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

import scala.collection.mutable.{ ListBuffer, Map, MultiDict }

import cell.CellUpdater
import handler.DependencyHandler
import handler.InitializationHandler

class Handler private[rasync]:
  val cells: ListBuffer[CellUpdater[?]] = ListBuffer()

  val initializers: Map[
    CellUpdater[?],
    InitializationHandler[?]
  ] = Map()

  val dependencies: MultiDict[
    CellUpdater[?],
    DependencyHandler[?, ?, ?]
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
          case Update(value)  => cell.update(value.asInstanceOf[cell.Value])
          case Complete(None) => cell.complete()
          case Complete(Some(value)) =>
            cell.update(value.asInstanceOf[cell.Value])
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
          case Update(value)  => dependent.update(value.asInstanceOf[dependent.Value])
          case Complete(None) => dependent.complete()
          case Complete(Some(value)) =>
            dependent.update(value.asInstanceOf[dependent.Value])
            dependent.complete()
          case Nothing =>
      }
