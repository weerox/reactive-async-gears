package rasync

import scala.collection.immutable.MultiDict

import gears.async.default.given
import gears.async.{ Async, Future }

import cell.{ Cell, CellUpdater }
import handler.DependencyHandler

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V] private[rasync] (val lattice: Lattice[V]):
  var cells: List[CellUpdater[V]] = List()

  // Mapping from a cell to the handlers which has that cell as a dependency.
  var handlers: MultiDict[Cell[V], DependencyHandler[V, ?, ?]] = MultiDict.empty

  // Cells which have been updated since the handlers where they are a dependency last ran.
  var updated: Set[Cell[V]] = Set()

  def registerUpdate(cell: Cell[V]): Unit =
    updated = updated + cell

  def registerDependencyHandler(handler: DependencyHandler[V, ?, ?]): Unit =
    for cell <- handler.dependencies do
      handlers = handlers + (cell -> handler)

  def deregisterDependencyHandler(handler: DependencyHandler[V, ?, ?]): Unit =
    for cell <- handler.dependencies do
      handlers = handlers - (cell -> handler)

  def initialize(): Unit =
    Async.blocking:
      cells
        .map(cell => cell.initializer)
        .flatten
        .map(handler =>
          Future:
            val result =
              try
                Right(handler.run())
              catch
                case e => Left(e)
            (handler.cell, result)
        )
        .awaitAll
        .map((cell, result) =>
          result match
            case Left(e) => cell.fail(e)
            case Right(outcome) => outcome match
                case Update(value) => cell.update(value)
                case Complete(value) =>
                  cell.update(value)
                  cell.complete()
        )

  def run(): Unit =
    var handlers: Seq[DependencyHandler[V, ?, ?]] = Seq.empty

    while
      handlers = updated.flatMap(cell => this.handlers.get(cell)).toSeq
      updated = Set.empty
      handlers.nonEmpty
    do
      Async.blocking:
        handlers
          .map(handler =>
            Future:
              val result =
                try
                  Right(handler.run())
                catch
                  case e => Left(e)
              (handler.dependent, result)
          )
          .awaitAll
          .map((dependent, result) =>
            result match
              case Left(e) => dependent.fail(e)
              case Right(outcome) => outcome match
                  case Update(value) => dependent.update(value)
                  case Complete      => dependent.complete()
                  case Complete(value) =>
                    dependent.update(value)
                    dependent.complete()
                  case Nothing =>
          )
