package rasync
package util

import gears.async.Async.OriginalSource
import gears.async.Listener

import scala.collection.mutable

import handler.DependencyHandler

private[rasync] class DependencySource[V] extends HandlerSource[DependencyHandler[V, ?, ?]]:
  override def extract(): Iterable[DependencyHandler[V, ?, ?]] =
    synchronized:
      val (uninit, init) = scheduled.partition(handler => handler.dependent.isUninitialized)
      scheduled.clear()
      scheduled ++= uninit
      init.toSeq
