package rasync

sealed trait Outcome[+V]
case class Update[V](value: V) extends Outcome[V]
case class Complete[V](value: Option[V]) extends Outcome[V]
case object Nothing extends Outcome
