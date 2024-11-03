package rasync
package handler

import gears.async.Async

private[rasync] trait Handler[Params, Result]:
  val handler: Params => Async ?=> Result

  def run()(using Async): Result
