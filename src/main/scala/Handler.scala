package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

import scala.collection.mutable.{ ListBuffer, Map, MultiDict }

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
      /*
        If a handler A for a dependent D is scheduled to run, then there is
        (almost) no point in running a handler B for dependent E which contain
        D as a dependency, since after handler A has completed, we might have a
        new value for D, which would require handler B to run again.
        So we track what dependents we have scheduled handlers for,
        so that we don't run handlers unneccesarily.
       */

      import scala.collection.mutable.Set
      val set: Set[Cell[?]] = Set()

      val (dependents, handlers) = dependencies.filter((dependent, handler) =>
        if
          /* none of the dependencies in the handler is in the set */
          handler.dependencies.forall(dependency => !set.contains(dependency))
        then
          /* add `dependent` to the set and schedule handler to run */
          set.add(dependent)
          true
        else
          /* at least one of dependencies are in the set, so we won't schedule the handler */
          false
      ).toSeq.unzip

      println(s"${dependencies.size}, ${handlers.size}")

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
