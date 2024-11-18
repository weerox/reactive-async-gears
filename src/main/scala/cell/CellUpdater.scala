package rasync
package cell

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

import gears.async.Async

import util.{ Container, ContainerMap }
import handler.InitializationHandler
import handler.DependencyHandler
import handler.SingletonDependencyHandler
import handler.IterableDependencyHandler
import handler.TupleDependencyHandler

// A `CellUpdater` is created using the methods defined in its companion object.
private[rasync] class CellUpdater[V] private (using handler: Handler[V])
    extends Cell[V], When[V]:

  // The state is initialized to `null`, but is set to `Uninitialized`
  // by all methods in the companion object.
  private val _state: AtomicReference[State[V]] = AtomicReference(null)
  override def state: State[V]                  = _state.get()

  override def get: V = _state.get() match
    case Value(value)      => value
    case Failed(exception) => throw exception
    case Uninitialized()   => throw Exception("cell is uninitialized")

  override def isCompleted(): Boolean = _state.get() match
    case Completed(_) => true
    case _            => false

  override def isFailed(): Boolean = _state.get() match
    case Failed(_) => true
    case _         => false

  override def hasValue(): Boolean = _state.get() match
    case Value(_) => true
    case _        => false

  def initializer: Option[InitializationHandler[V]] = _state.get() match
    case state: Uninitialized[V] => Some(state.initializer)
    case _                       => None

  def dependencies: Set[DependencyHandler[V, ?, ?]] = _state.get() match
    case s: Uninitialized[V] => s.dependencies
    case s: Intermediate[V]  => s.dependencies
    case _                   => Set()

  @tailrec
  private def addDependency(handler: DependencyHandler[V, ?, ?]): Unit = _state.get() match
    case state: Uninitialized[V] =>
      if _state.compareAndSet(
          state,
          new Uninitialized(state.initializer, state.dependencies + handler)
        )
      then this.handler.registerDependencyHandler(handler)
      else addDependency(handler)
    case state: Intermediate[V] =>
      if _state.compareAndSet(
          state,
          new Intermediate(state.value, state.dependencies + handler)
        )
      then this.handler.registerDependencyHandler(handler)
      else addDependency(handler)
    // TODO Throw error?
    case _ =>

  @tailrec
  private[rasync] final def removeDependency(handler: DependencyHandler[V, ?, ?]): Unit =
    _state.get() match
      case state: Uninitialized[V] =>
        if _state.compareAndSet(
            state,
            new Uninitialized(state.initializer, state.dependencies - handler)
          )
        then this.handler.deregisterDependencyHandler(handler)
        else removeDependency(handler)
      case state: Intermediate[V] =>
        if _state.compareAndSet(
            state,
            new Intermediate(state.value, state.dependencies - handler)
          )
        then this.handler.deregisterDependencyHandler(handler)
        else removeDependency(handler)
      case _ =>

  override def when(dependencies: Iterable[Cell[V]])(
      body: Iterable[State[V]] => Async ?=> Outcome[V]
  ): Unit = addDependency(IterableDependencyHandler(this, dependencies, body))

  override def when(dependencies: Cell[V])(
      body: State[V] => Async ?=> Outcome[V]
  ): Unit = addDependency(SingletonDependencyHandler(this, dependencies, body))

  override def when[
      Args <: Tuple: Container[Cell],
      Params <: ContainerMap[Args, Cell, State]
  ](dependencies: Args)(
      body: Params => Async ?=> Outcome[V]
  ): Unit = addDependency(TupleDependencyHandler(this, dependencies, body))

  @tailrec
  final def update(value: V): Unit = _state.get() match
    case state: Uninitialized[V] =>
      if _state.compareAndSet(
          state,
          new Intermediate(value, state.dependencies)
        )
      then handler.registerUpdate(this)
      else update(value)
    case state: Intermediate[V] =>
      val next = handler.lattice.join(state.value, value)
      if state.value != next then
        if _state.compareAndSet(
            state,
            new Intermediate(next, state.dependencies)
          )
        then handler.registerUpdate(this)
        else update(value)
    case _ =>

  @tailrec
  final def complete(): Unit =
    val current = _state.get()
    current match
      case Intermediate(value) =>
        if _state.compareAndSet(current, Completed(value))
        then handler.registerUpdate(this)
        else complete()
      // NOTE Should this update the state to Failed instead of throwing?
      case Uninitialized() => throw Exception("tried to complete a cell without a value")
      case _               =>

  @tailrec
  final def fail(exception: Throwable): Unit =
    val current = _state.get()
    current match
      case Intermediate(_) | Uninitialized() =>
        if _state.compareAndSet(current, Failed(exception))
        then
          // It seems reasonable to register a update here since the state
          // actually changed. Someone might use the fact that the cell failed in
          // a dependency handler.
          handler.registerUpdate(this)
        else fail(exception)
      // If the cell is already completed or failed, then we won't fail it.
      case Completed(_) | Failed(_) =>

object CellUpdater:
  def initial[V](initial: Update[V] | Complete[V])(using Handler[V]): CellUpdater[V] =
    val cell = new CellUpdater
    cell._state.set(Uninitialized(InitializationHandler(cell, initial)))
    cell

  def initializer[V](initializer: Async ?=> Update[V] | Complete[V])(using
      Handler[V]
  ): CellUpdater[V] =
    val cell = new CellUpdater
    cell._state.set(Uninitialized(InitializationHandler(cell, initializer)))
    cell
