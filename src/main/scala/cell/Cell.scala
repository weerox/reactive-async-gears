package rasync
package cell

trait Cell[V]:
  def get: V
  def state: State[V]

  def isUninitialized = state.isUninitialized
  def isCompleted     = state.isCompleted
  def isFailed        = state.isFailed
  def hasValue        = state.hasValue
