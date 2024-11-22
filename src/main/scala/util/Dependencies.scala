package rasync
package util

import gears.async.Async.OriginalSource
import gears.async.Listener

import scala.collection.mutable

import handler.DependencyHandler

private[rasync] class Dependencies[V]
    extends OriginalSource[Option[Iterable[DependencyHandler[V, ?, ?]]]]:

  private type Value = Option[Iterable[DependencyHandler[V, ?, ?]]]

  var listener: Option[Listener[Value]] = None

  val scheduled: mutable.Set[DependencyHandler[V, ?, ?]] = mutable.Set.empty

  var stopped = false

  override def addListener(k: Listener[Value]): Unit =
    synchronized:
      if listener.isEmpty then listener = Some(k)

  override def dropListener(k: Listener[Value]): Unit =
    synchronized:
      if listener.isDefined && listener.get == k then listener = None

  override def poll(k: Listener[Value]): Boolean =
    synchronized:
      if scheduled.nonEmpty then
        val (uninit, init) = scheduled.partition(handler => handler.dependent.isUninitialized)
        scheduled.clear()
        scheduled ++= uninit
        if init.nonEmpty then
          k.completeNow(Some(init.toSeq), this)
        else false
      else
        if stopped then
          k.completeNow(None, this)
        else false

  /** Schedule new dependency handlers to be executed. */
  def schedule(handlers: Iterable[DependencyHandler[V, ?, ?]]): Unit =
    synchronized:
      scheduled ++= handlers
      if listener.isDefined then
        val (uninit, init) = scheduled.partition(handler => handler.dependent.isUninitialized)
        if init.nonEmpty then
          listener.get.completeNow(Some(init.toSeq), this)
          scheduled.clear()
          scheduled ++= uninit
          listener = None

  /** Signal that trying to wait for new dependency handlers should fail.
    *
    * This must only be called when it is guaranteed that dependency handlers cannot be concurrently
    * scheduled, i.e. after setup is done and after all cell initializers have been executed.
    */
  def stop(): Unit =
    synchronized:
      stopped = true
      if listener.isDefined then
        if scheduled.nonEmpty then
          // All cells should have been initialized when calling this method,
          // so all dependency handlers will have dependent cells that are initialized.
          listener.get.completeNow(Some(scheduled.toSeq), this)
          scheduled.clear()
          listener = None
        else
          listener.get.completeNow(None, this)
          listener = None
