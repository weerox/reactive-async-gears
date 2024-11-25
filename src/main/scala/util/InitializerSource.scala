package rasync
package util

import gears.async.Async.OriginalSource
import gears.async.Listener

import scala.collection.mutable

import handler.InitializationHandler

private[rasync] class InitializerSource[V]
    extends HandlerSource[InitializationHandler[V]]:

  override def extract(): Iterable[InitializationHandler[V]] =
    val result = scheduled.toSeq
    scheduled.clear()
    result
