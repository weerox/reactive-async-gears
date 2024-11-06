package rasync
package handler

import gears.async.Async

private[rasync] trait Handler[Result]:
  def run()(using Async): Result
