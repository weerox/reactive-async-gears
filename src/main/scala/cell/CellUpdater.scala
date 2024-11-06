package rasync
package cell

import gears.async.Async

import util.{ Container, ContainerMap }
import handler.SingletonDependencyHandler
import handler.IterableDependencyHandler
import handler.TupleDependencyHandler

private[rasync] class CellUpdater[V](using handler: Handler[V]) extends Cell[V]:
  private var _state: State[V] = Intermediate(handler.lattice.bottom)
  override def state: State[V] = _state

  override def get: V = _state match
    case Value(value)      => value
    case Failed(exception) => throw exception

  override def isComplete(): Boolean = _state match
    case Completed(_) => true
    case _            => false

  override def when(dependencies: Iterable[Cell[V]])(
      body: Iterable[State[V]] => Async ?=> Outcome[V]
  ): Unit = handler.dependencies += this -> IterableDependencyHandler(dependencies, body)

  override def when(dependencies: Cell[V])(
      body: State[V] => Async ?=> Outcome[V]
  ): Unit = handler.dependencies += this -> SingletonDependencyHandler(dependencies, body)

  override def when[
      Args <: Tuple: Container[Cell],
      Params <: ContainerMap[Args, Cell, State]
  ](dependencies: Args)(
      body: Params => Async ?=> Outcome[V]
  ): Unit = handler.dependencies += this -> TupleDependencyHandler(dependencies, body)

  def update(value: V): Unit = _state match
    case Intermediate(current) =>
      _state = Intermediate(handler.lattice.join(current, value))
    case _ =>

  def complete(): Unit = _state match
    case Intermediate(value) =>
      _state = Completed(value)
      handler.dependencies.removeKey(this)
    case _ =>

  def fail(exception: Throwable): Unit = _state match
    case Intermediate(_) =>
      _state = Failed(exception)
      handler.dependencies.removeKey(this)
    // If the cell is already completed or failed, then we won't fail it.
    case Completed(_) | Failed(_) =>
