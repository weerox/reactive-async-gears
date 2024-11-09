package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

import cell.CellUpdater

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V] private[rasync] (val lattice: Lattice[V]):
  var cells: List[CellUpdater[V]] = List()

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
                case Complete      => cell.complete()
                case Complete(value) =>
                  cell.update(value)
                  cell.complete()
                case Nothing =>
        )

  def run(): Unit =
    def dependencies = cells.flatMap(cell => cell.dependencies)

    while
      !dependencies.isEmpty
    do
      Async.blocking:
        dependencies
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
