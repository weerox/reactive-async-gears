package rasync

import scala.collection.mutable.MultiDict

private[rasync] class CellUpdater[V](using handler: Handler[V]) extends Cell[V]:
  private[rasync] var dependencies: MultiDict[
    Iterable[Cell[V]],
    Iterable[State[V]] => Outcome[V]
  ] = MultiDict()

  private var _state: State[V] = Intermediate(handler.lattice.bottom)
  override def state: State[V] = _state

  override def get: V = state match
    case Intermediate(value) => value
    case Completed(value)    => value
    case Failed(exception)   => throw exception

  override def isComplete(): Boolean = state match
    case Completed(_) => true
    case _            => false

  override def when(dependencies: Iterable[Cell[V]])(
      body: Iterable[State[V]] => Outcome[V]
  ): Unit = this.dependencies += dependencies -> body

  def update(value: V): Unit = state match
    case Intermediate(current) =>
      _state = Intermediate(handler.lattice.join(current, value))
    case _ =>

  def complete(): Unit = state match
    case Intermediate(value) =>
      _state = Completed(value)
      dependencies.clear()
    case _ =>
