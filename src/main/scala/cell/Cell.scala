package rasync
package cell

trait Cell[V]:
  def get: V
  def state: State[V]

  def isCompleted(): Boolean
  def isFailed(): Boolean
  def hasValue(): Boolean
