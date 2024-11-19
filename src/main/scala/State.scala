package rasync

import handler.{ DependencyHandler, InitializationHandler }

sealed trait State[V]:
  def isUninitialized: Boolean
  def isCompleted: Boolean
  def isFailed: Boolean
  def hasValue: Boolean

case class Uninitialized[V] private[rasync] (
    private[rasync] val initializer: InitializationHandler[V],
    private[rasync] val dependencies: Set[DependencyHandler[V, ?, ?]]
) extends State[V]:
  def isUninitialized = true
  def isCompleted     = false
  def isFailed        = false
  def hasValue        = false

case class Intermediate[V] private[rasync] (
    value: V,
    private[rasync] val dependencies: Set[DependencyHandler[V, ?, ?]]
) extends State[V]:
  def isUninitialized = false
  def isCompleted     = false
  def isFailed        = false
  def hasValue        = true

case class Completed[V](value: V) extends State[V]:
  def isUninitialized = false
  def isCompleted     = true
  def isFailed        = false
  def hasValue        = true

case class Failed[V](exception: Throwable) extends State[V]:
  def isUninitialized = false
  def isCompleted     = false
  def isFailed        = true
  def hasValue        = false

object Uninitialized:
  def unapply[V](uninitialized: Uninitialized[V]): true = true

object Intermediate:
  def unapply[V](intermediate: Intermediate[V]): Some[V] = Some(intermediate.value)

object Value:
  def unapply[V](x: Intermediate[V] | Completed[V]): Some[V] = x match
    case Intermediate(value) => Some(value)
    case Completed(value)    => Some(value)
