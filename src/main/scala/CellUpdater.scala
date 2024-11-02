package rasync

import scala.collection.mutable.MultiDict

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
      body: Iterable[State[V]] => Outcome[V]
  ): Unit =
    handler.dependencies.getOrElseUpdate(this, MultiDict()) += dependencies -> body

  def update(value: V): Unit = _state match
    case Intermediate(current) =>
      _state = Intermediate(handler.lattice.join(current, value))
    case _ =>

  def complete(): Unit = _state match
    case Intermediate(value) =>
      _state = Completed(value)
      handler.dependencies.get(this) match
        case Some(map) => map.clear()
        case _         =>
    case _ =>
