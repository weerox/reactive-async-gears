package rasync

private[rasync] class CellUpdater[V](using handler: Handler[V]) extends Cell[V]:
	private val value: V = ???

	override def get: V = value
	override def isComplete(): Boolean = ???

	def update(value: V): Unit = ???
	def complete(): Unit = ???
