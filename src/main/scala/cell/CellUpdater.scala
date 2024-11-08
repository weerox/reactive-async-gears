package rasync
package cell

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
  private var _state: State[V] = null
  override def state: State[V] = _state

  override def get: V = _state match
    case Value(value)      => value
    case Failed(exception) => throw exception
    case Uninitialized()   => throw Exception("cell is uninitialized")

  override def isCompleted(): Boolean = _state match
    case Completed(_) => true
    case _            => false

  override def isFailed(): Boolean = _state match
    case Failed(_) => true
    case _         => false

  override def hasValue(): Boolean = _state match
    case Value(_) => true
    case _        => false

  def initializer: Option[InitializationHandler[V]] = _state match
    case state: Uninitialized[V] => Some(state.initializer)
    case _                       => None

  def dependencies: Set[DependencyHandler[V, ?, ?]] = _state match
    case s: Uninitialized[V] => s.dependencies
    case s: Intermediate[V]  => s.dependencies
    case _                   => Set()

  private def addDependency(handler: DependencyHandler[V, ?, ?]): Unit = _state match
    case state: Uninitialized[V] =>
      _state = new Uninitialized(state.initializer, state.dependencies + handler)
    case state: Intermediate[V] =>
      _state = new Intermediate(state.value, state.dependencies + handler)
    // TODO Throw error?
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

  def update(value: V): Unit = _state match
    case state: Uninitialized[V] =>
      _state = new Intermediate(value, state.dependencies)
    case Intermediate(current) =>
      _state = Intermediate(handler.lattice.join(current, value))
    case _ =>

  def complete(): Unit = _state match
    case Intermediate(value) =>
      _state = Completed(value)
    // NOTE Should this update the state to Failed instead of throwing?
    case Uninitialized() => throw Exception("tried to complete a cell without a value")
    case _               =>

  def fail(exception: Throwable): Unit = _state match
    case Intermediate(_) | Uninitialized() =>
      _state = Failed(exception)
    // If the cell is already completed or failed, then we won't fail it.
    case Completed(_) | Failed(_) =>

object CellUpdater:
  def initial[V](initial: Outcome[V])(using Handler[V]): CellUpdater[V] =
    val cell = new CellUpdater
    cell._state = Uninitialized(InitializationHandler(cell, initial))
    cell

  def initializer[V](initializer: Async ?=> Outcome[V])(using Handler[V]): CellUpdater[V] =
    val cell = new CellUpdater
    cell._state = Uninitialized(InitializationHandler(cell, initializer))
    cell