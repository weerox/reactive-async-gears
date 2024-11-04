package rasync
package cell

import gears.async.Async

import util.{ Container, ContainerMap }
import handler.SingletonDependencyHandler
import handler.IterableDependencyHandler
import handler.TupleDependencyHandler

private[rasync] class CellUpdater[V](using handler: Handler, lattice: Lattice[V]) extends Cell[V]:
  type Value = V

  private var _state: State[V] = Intermediate(lattice.bottom)
  override def state: State[V] = _state

  override def get: V = _state match
    case Value(value)      => value
    case Failed(exception) => throw exception

  override def isComplete(): Boolean = _state match
    case Completed(_) => true
    case _            => false

  override def when[T](dependencies: Iterable[Cell[T]])(
      body: Iterable[State[T]] => Async ?=> Outcome[V]
  ): Unit = handler.dependencies += this -> IterableDependencyHandler(dependencies, body)

  override def when[T](dependencies: Cell[T])(
      body: State[T] => Async ?=> Outcome[V]
  ): Unit = handler.dependencies += this -> SingletonDependencyHandler(dependencies, body)

  override def when[
      Args <: Tuple: Container[Cell],
      Params <: ContainerMap[Args, Cell, State]
  ](dependencies: Args)(
      body: Params => Async ?=> Outcome[V]
  ): Unit = handler.dependencies += this -> TupleDependencyHandler(dependencies, body)

  def update(value: V): Unit = _state match
    case Intermediate(current) =>
      _state = Intermediate(lattice.join(current, value))
    case _ =>

  def complete(): Unit = _state match
    case Intermediate(value) =>
      _state = Completed(value)
      handler.dependencies.removeKey(this)
    case _ =>
