package rasync
package util

import scala.collection.mutable.Queue

import gears.async.Channel
import gears.async.Channel.Closed
import gears.async.Async.Source
import gears.async.Listener
import gears.async.ChannelClosedException

/** A channel that can buffer an unbounded queue of items.
  *
  * This channel implementation is inspired by the [[gears.async.UnboundedChannel]] but you will be
  * able to read from this channel after it it closed as long as there are items left in the queue.
  */
private[rasync] class QueueChannel[T] extends Channel[T]:

  type Res[T] = Either[Closed, T]

  private val queue: Queue[T] = Queue.empty

  private val readers: Queue[Reader] = Queue.empty

  private var isClosed = false

  override val readSource: Source[Res[T]] = new Source[Res[T]]:
    override def poll(k: Listener[Res[T]]): Boolean =
      // If these values are simultaneously true at any point in time,
      // they will always continue to be true, so we don't have to check
      // this condition synchronously.
      if isClosed && queue.isEmpty then
        k.completeNow(Left(Closed), this)
      else
        QueueChannel.this.synchronized:
          if queue.nonEmpty then
            k.completeNow(Right(queue.dequeue()), this)
            true
          else
            false

    override def onComplete(k: Listener[Res[T]]): Unit =
      QueueChannel.this.synchronized:
        if poll(k) == false then readers.enqueue(k)

    override def dropListener(k: Listener[Res[T]]): Unit =
      QueueChannel.this.synchronized:
        readers.removeAll(r => r == k)

  override def sendSource(x: T): Source[Res[Unit]] = new Source[Res[Unit]]:
    override def poll(k: Listener[Res[Unit]]): Boolean =
      val result =
        if isClosed then
          Left(Closed)
        else
          QueueChannel.this.synchronized:
            if readers.nonEmpty then
              readers.dequeue.completeNow(Right(x), readSource)
            else
              queue.enqueue(x)
            Right(())
      k.completeNow(result, this)
    override def onComplete(k: Listener[Res[Unit]]): Unit   = poll(k)
    override def dropListener(k: Listener[Res[Unit]]): Unit = ()

  override def close(): Unit =
    synchronized:
      if !isClosed then
        isClosed = true
        // No new values can be sent, so if the queue is empty but we have queued readers,
        // then the readers must be notified that the channel is closed.
        if readers.nonEmpty && queue.isEmpty then
          readers.foreach(r => r.completeNow(Left(Closed), readSource))
          readers.clear()

  /** Send the item immediately. */
  def sendImmediately(x: T): Unit =
    var result: Res[Unit] = null
    sendSource(x).poll(Listener((r, _) => result = r))
    if result.isLeft then throw ChannelClosedException()
