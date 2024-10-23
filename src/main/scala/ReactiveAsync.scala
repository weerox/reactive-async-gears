package rasync

import gears.async.default.given
import gears.async.{ Async, Future }

object ReactiveAsync:
	def handler[V](body: Handler[V] ?=> Unit): Handler[V] =
		val handler = Handler[V]()
		body(using handler)
		Async.blocking:
			val (cells, inits) = handler.initializers.toList.unzip
			val results = inits.map { init =>
				Future:
					init()
			}.awaitAll
			cells.zip(results).map((cell, result) => cell.update(result))
		handler

	def cell[V](using handler: Handler[V]): Cell[V] =
		val cell = CellUpdater[V]()
		handler.cells += cell
		cell

	def cell[V](init: () => Async ?=> V)(using handler: Handler[V]): Cell[V] =
		val cell = CellUpdater[V]()
		handler.cells += cell
		// For some reason, Scala doesn't like it if the tuple is used directly
		// in the assignment expression.
		val tmp = (cell, init)
		handler.initializers += tmp
		cell