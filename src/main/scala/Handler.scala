package rasync

import scala.collection.immutable.MultiDict

import java.util.concurrent.atomic.AtomicReference

import gears.async.{ Async, Future }

import cell.{ Cell, CellUpdater }
import handler.DependencyHandler
import handler.InitializationHandler
import util.QueueChannel
import util.Dependencies

/*
 A handler is restricted to hold cells with value V.
 It would be kinda nice to allow a handler to hold cells of different values,
 but I'm not sure that is safe to do/the type system won't like it.
 */
class Handler[V] private[rasync] (val lattice: Lattice[V]):
  private var cells: List[CellUpdater[V]] = List()

  // Mapping from a cell to the handlers which has that cell as a dependency.
  private val handlers: AtomicReference[MultiDict[Cell[V], DependencyHandler[V, ?, ?]]] =
    AtomicReference(MultiDict.empty)

  // A source of dependencies that are scheduled to execute.
  private val dependencies = Dependencies[V]()

  def registerCell(cell: CellUpdater[V]): Unit =
    cells = cell :: cells
    cell.initializer match
      case Some(initializer) => nextInitializer.sendImmediately(initializer)
      // This case should never happen, since cells are only registered when
      // they are in an uninitialized state.
      case None =>

  def registerUpdate(cell: Cell[V]): Unit =
    val h = handlers.get().get(cell)
    if h.nonEmpty then dependencies.schedule(h)

  def registerDependencyHandler(handler: DependencyHandler[V, ?, ?]): Unit =
    handlers.getAndUpdate(handlers =>
      handler.dependencies.foldLeft(handlers)((handlers, cell) =>
        handlers + (cell -> handler)
      )
    )

    // The cells that this dependency handler has as dependencies might have
    // already been completed (or won't get more updates for some other reason).
    // That would mean that this dependency handler would never get to run,
    // so to make sure that it will run at least once, we will manually schedule it.
    // NOTE This has to be done *after* we have added the reverse mappings for the dependencies.
    // If the dependency handler was scheduled *before*, then we might run this handler
    // *before* the reverse mappings are added. That could lead to the following chain of events:
    // 1. We manually schedule this dependency handler.
    // 2. This dependency handler is executed.
    // 3. All dependencies of this dependency handler are completed.
    // 4. The reverse mappings are registered.
    // This means that we can't guarantee that a dependency handler will run on the final state
    // of its dependencies.
    dependencies.schedule(Iterable.single(handler))

  def deregisterDependencyHandler(handler: DependencyHandler[V, ?, ?]): Unit =
    handlers.getAndUpdate(handlers =>
      handler.dependencies.foldLeft(handlers)((handlers, cell) => handlers - (cell -> handler))
    )

  // Initializers are sent to this channel.
  private[rasync] val nextInitializer = QueueChannel[InitializationHandler[V]]
  def execute_initializers()(using Async): Unit =
    for
      initializers <- Iterator
        .continually(nextInitializer.readAll())
        .takeWhile(_.isRight)
        .collect { case Right(v) => v }
    do
      Async.group:
        initializers
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

  def execute_dependencies()(using Async): Unit =
    for
      handlers <- Iterator
        .continually(dependencies.awaitResult)
        .takeWhile(_.isDefined)
        .flatten
        .map(_.toSeq)
    do
      Async.group:
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

  def execute()(using Async): Unit =
    Async.group:
      val init = Future:
        execute_initializers()
      val deps = Future:
        execute_dependencies()
      init.await
      _done.init = true
      dependencies.stop()
      deps.await
      _done.deps = true

  class Done:
    var setup = false
    var init  = false
    var deps  = false
  val _done = Done()

  def done(): Unit =
    _done.setup = true
    nextInitializer.close()
