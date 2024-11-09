package rasync

sealed trait Outcome[+V]
case class Update[V](value: V)   extends Outcome[V]
case class Complete[V](value: V) extends Outcome[V]
case object Complete             extends Outcome
case object Nothing              extends Outcome
