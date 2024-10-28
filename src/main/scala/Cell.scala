package rasync

trait Cell[V]:
  def get: V
  def state: State[V]
  def isComplete(): Boolean
