package rasync

import gears.async.Async

import cell.{ Cell, CellUpdater }
import handler.InitializationHandler

object ReactiveAsync:
  def handler[T](body: Handler ?=> T): T =
    val handler = Handler()
    val result  = body(using handler)
    handler.initialize()
    handler.run()
    result

  def cell[V](using handler: Handler, lattice: Lattice[V]): Cell[V] =
    val cell = CellUpdater[V]()
    handler.cells += cell
    cell

  def cell[V](init: () => Async ?=> Outcome[V])(using
      handler: Handler,
      lattice: Lattice[V]
  ): Cell[V] =
    val cell = CellUpdater[V]()
    handler.cells += cell
    handler.initializers += cell -> InitializationHandler(init)
    cell
