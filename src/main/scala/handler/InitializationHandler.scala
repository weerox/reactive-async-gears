package rasync
package handler

import gears.async.Async

private[rasync] class InitializationHandler[V](
    val handler: Async ?=> Outcome[V]
) extends Handler[Outcome[V]]:
  def run()(using Async): Outcome[V] = handler
