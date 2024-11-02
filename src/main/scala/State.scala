package rasync

sealed trait State[+V]
case class Intermediate[V](value: V)    extends State[V]
case class Completed[V](value: V)       extends State[V]
case class Failed(exception: Throwable) extends State

object Value:
  def unapply[V](x: Intermediate[V] | Completed[V]): Some[V] = x match
    case Intermediate(value) => Some(value)
    case Completed(value)    => Some(value)
