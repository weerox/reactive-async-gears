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
  ): Unit =
    handler.dependencies += this -> IterableDependencyHandler(dependencies, body)
    dependencies.foreach(cell =>
      handler.dependents += cell -> this
    )

  override def when(dependencies: Cell[V])(
      body: State[V] => Async ?=> Outcome[V]
  ): Unit =
    handler.dependencies += this       -> SingletonDependencyHandler(dependencies, body)
    handler.dependents += dependencies -> this

  override def when[
      Args <: Tuple: Container[Cell],
      Params <: ContainerMap[Args, Cell, State]
  ](dependencies: Args)(
      body: Params => Async ?=> Outcome[V]
  ): Unit =
    handler.dependencies += this -> TupleDependencyHandler(dependencies, body)
    // NOTE I'm not quite sure that a cell in a tuple will always be of type `Cell[V]`,
    // but if it where anything else it would be wrong, since the `Handler` only accepts
    // cells with values of type `V`.
    dependencies.productIterator.asInstanceOf[Iterator[Cell[V]]].foreach(cell =>
      handler.dependents += cell -> this
    )

  def update(value: V): Unit = _state match
    case Intermediate(current) =>
      _state = Intermediate(handler.lattice.join(current, value))
    case _ =>

  def complete(): Unit = _state match
    case Intermediate(value) =>
      _state = Completed(value)
      handler.dependencies.removeKey(this)
    case _ =>
