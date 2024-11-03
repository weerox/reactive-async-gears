package rasync

import cell.{ Cell, CellUpdater }

import gears.async.Async

object ReactiveAsync:
  def handler[V, T](body: Handler[V] ?=> T)(using lattice: Lattice[V]): T =
    val handler = Handler[V](lattice)
    val result  = body(using handler)
    handler.initialize()
    handler.run()
    result

  def cell[V](using handler: Handler[V]): Cell[V] =
    val cell = CellUpdater[V]()
    handler.cells += cell
    cell

  def cell[V](init: () => Async ?=> Outcome[V])(using
      handler: Handler[V]
  ): Cell[V] =
    val cell = CellUpdater[V]()
    handler.cells += cell
    handler.initializers += cell -> init
    cell
