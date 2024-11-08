package rasync
package handler

import gears.async.Async

import cell.CellUpdater

private[rasync] class InitializationHandler[V](
    val cell: CellUpdater[V],
    val handler: Async ?=> Outcome[V]
) extends Handler[Outcome[V]]:
  def run()(using Async): Outcome[V] = handler
