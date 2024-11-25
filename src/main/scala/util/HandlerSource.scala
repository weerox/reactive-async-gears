package rasync
package util

import gears.async.Async.OriginalSource
import gears.async.Listener

import scala.collection.mutable

import handler.Handler

private[rasync] abstract class HandlerSource[H <: Handler[?]]
    extends OriginalSource[Option[Iterable[H]]]:

  protected type Value = Option[Iterable[H]]

  var listener: Option[Listener[Value]] = None

  val scheduled: mutable.Set[H] = mutable.Set.empty

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
        val data = extract()
        if data.nonEmpty then
          k.completeNow(Some(data), this)
        else false
      else
        if stopped then
          k.completeNow(None, this)
        else false

  /** Extract handlers that can be executed.
    *
    * Make sure to remove the returned handlers from the set of scheduled handlers.
    */
  protected def extract(): Iterable[H]

  /** Schedule a new handler to be executed. */
  def schedule(handler: H): Unit =
    synchronized:
      scheduled += handler
      if listener.isDefined then
        val data = extract()
        if data.nonEmpty then
          listener.get.completeNow(Some(data), this)
          listener = None
        // TODO Create a method that subclasses use to do this action
        // val (uninit, init) = scheduled.partition(handler => handler.dependent.isUninitialized)
        /*
        if init.nonEmpty then
          listener.get.completeNow(Some(init.toSeq), this)
          scheduled.clear()
          scheduled ++= uninit
          listener = None
         */

  /** Schedule new handlers to be executed. */
  def schedule(handlers: Iterable[H]): Unit =
    synchronized:
      scheduled ++= handlers
      if listener.isDefined then
        val data = extract()
        if data.nonEmpty then
          listener.get.completeNow(Some(data), this)
          listener = None

  /** Signal that trying to wait for new handlers should fail.
    *
    * This must only be called when it is guaranteed that handlers cannot be concurrently scheduled,
    * i.e. initialization handlers won't be concurrently scheduled after setup is done and
    * dependency handlers won't be concurrently scheduled after all cell initializers have been
    * executed.
    *
    * Note that it is still okay to scheduled handlers, but you have to do so *before* trying to
    * wait for handlers.
    */
  def stop(): Unit =
    synchronized:
      stopped = true
      if listener.isDefined then
        if scheduled.nonEmpty then
          listener.get.completeNow(Some(scheduled.toSeq), this)
          scheduled.clear()
          listener = None
        else
          listener.get.completeNow(None, this)
          listener = None
