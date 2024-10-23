package rasync

trait Cell[V]:
	def get: V
	def isComplete(): Boolean