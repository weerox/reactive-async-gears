package rasync

private[rasync] class CellUpdater[V](using handler: Handler[V]) extends Cell[V]:
  private var state: State[V] = Intermediate(handler.lattice.bottom)

  override def get: V = state match
    case Intermediate(value) => value
    case Completed(value)    => value
    case Failed(exception)   => throw exception

  override def isComplete(): Boolean = state match
    case Completed(_) => true
    case _            => false

  def update(value: V): Unit = state match
    case Intermediate(current) =>
      state = Intermediate(handler.lattice.join(current, value))
    case _ =>

  def complete(): Unit = state match
    case Intermediate(value) => state = Completed(value)
    case _                   =>
