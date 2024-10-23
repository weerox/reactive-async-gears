package rasync

private[rasync] class CellUpdater[V](using handler: Handler[V]) extends Cell[V]:
	private var value: V = handler.lattice.bottom

	override def get: V = value
	override def isComplete(): Boolean = ???

	def update(value: V): Unit = this.value = handler.lattice.join(this.value, value)
	def complete(): Unit = ???
