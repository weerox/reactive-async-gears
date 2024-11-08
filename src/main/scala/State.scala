package rasync

import handler.{ DependencyHandler, InitializationHandler }

sealed trait State[V]
case class Uninitialized[V] private[rasync] (
    private[rasync] val initializer: InitializationHandler[V],
    private[rasync] val dependencies: Set[DependencyHandler[V, ?, ?]]
) extends State[V]
case class Intermediate[V] private[rasync] (
    value: V,
    private[rasync] val dependencies: Set[DependencyHandler[V, ?, ?]]
) extends State[V]
case class Completed[V](value: V)          extends State[V]
case class Failed[V](exception: Throwable) extends State[V]

object Uninitialized:
  private[rasync] def apply[V](initializer: InitializationHandler[V]): Uninitialized[V] =
    new Uninitialized(initializer, Set())
  def unapply[V](uninitialized: Uninitialized[V]): true = true

object Intermediate:
  def apply[V](value: V): Intermediate[V]                = new Intermediate(value, Set())
  def unapply[V](intermediate: Intermediate[V]): Some[V] = Some(intermediate.value)

object Value:
  def unapply[V](x: Intermediate[V] | Completed[V]): Some[V] = x match
    case Intermediate(value) => Some(value)
    case Completed(value)    => Some(value)
