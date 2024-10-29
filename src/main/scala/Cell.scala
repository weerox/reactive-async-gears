package rasync

trait Cell[V]:
  def get: V
  def state: State[V]
  def isComplete(): Boolean

  def when(dependencies: Iterable[Cell[V]])(
      body: Iterable[State[V]] => Outcome[V]
  ): Unit
