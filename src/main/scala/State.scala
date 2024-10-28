package rasync

sealed trait State[+V]
case class Intermediate[V](value: V) extends State[V]
case class Completed[V](value: V) extends State[V]
case class Failed(exception: Throwable) extends State