package rasync

import gears.async.Async

import cell.{ Cell, CellUpdater, When }

object ReactiveAsync:
  def handler[V, T](body: Handler[V] ?=> T)(using lattice: Lattice[V]): T =
    val handler = Handler[V](lattice)
    val result  = body(using handler)
    handler.initialize()
    handler.run()
    result

  // I would have liked to name all of these methods `cell`,
  // but the compiler has a hard time figuring out which overload to pick.
  // Having different names probably makes it a bit clearer for developers too.

  /** Creates a cell with an initial value given by the lattice. */
  def cell[V](using handler: Handler[V]): Cell[V] & When[V] =
    val cell: CellUpdater[V] = CellUpdater.initial(Update(handler.lattice.bottom))
    handler.cells = cell +: handler.cells
    cell

  /** Creates a cell that completes with the given value. */
  def completed[V](value: V)(using handler: Handler[V]): Cell[V] & When[V] =
    val cell: CellUpdater[V] = CellUpdater.initial(Complete(value))
    handler.cells = cell +: handler.cells
    cell

  /** Creates a cell with an inital value given by the outcome. */
  def initial[V](initial: Update[V] | Complete[V])(using handler: Handler[V]): Cell[V] & When[V] =
    val cell: CellUpdater[V] = CellUpdater.initial(initial)
    handler.cells = cell +: handler.cells
    cell

  /** Creates a cell with an inital value given by the supplied initializer. */
  def initialize[V](initializer: Async ?=> Update[V] | Complete[V])(using
      handler: Handler[V]
  ): Cell[V] & When[V] =
    val cell: CellUpdater[V] = CellUpdater.initializer(initializer)
    handler.cells = cell +: handler.cells
    cell
