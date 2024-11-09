package rasync
package handler

import gears.async.Async

import cell.CellUpdater

private[rasync] class InitializationHandler[V](
    val cell: CellUpdater[V],
    val handler: Async ?=> Update[V] | Complete[V]
) extends Handler[Update[V] | Complete[V]]:
  def run()(using Async): Update[V] | Complete[V] = handler
