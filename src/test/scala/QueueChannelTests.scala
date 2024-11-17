package rasync
package test

import rasync.util.QueueChannel

import gears.async.Async
import gears.async.ChannelClosedException
import gears.async.Channel.Closed
import gears.async.Future
import gears.async.default.given

import util.rerun

class QueueChannelTests extends util.AsyncSuite:
  test("closed channel cannot be sent to".rerun(100)) {
    val channel = QueueChannel[Int]
    channel.close()
    intercept[ChannelClosedException] {
      channel.sendImmediately(0)
    }
  }

  test("closed channel with queued elements can be read".rerun(100)) {
    Async.blocking:
      val channel = QueueChannel[Int]
      channel.sendImmediately(0)
      channel.sendImmediately(1)
      channel.sendImmediately(2)
      channel.close()
      channel.read() match
        case Right(0) =>
        case _        => fail("expected to be able to read a 0")
      channel.read() match
        case Right(1) =>
        case _        => fail("expected to be able to read a 0")
      channel.read() match
        case Right(2) =>
        case _        => fail("expected to be able to read a 0")
      channel.read() match
        case Left(Closed) =>
        case _            => fail("expected the channel to be closed")
  }

  test("two readers can be active at the same time".rerun(100)) {
    Async.blocking:
      val channel = QueueChannel[Int]
      val a = Future:
        channel.read()
      val b = Future:
        channel.read()

      channel.sendImmediately(1)
      channel.sendImmediately(2)

      var sum = 0

      a.await match
        case Right(value) => sum += value
        case _            => fail("expected a value")
      b.await match
        case Right(value) => sum += value
        case _            => fail("expected a value")

      assertEquals(sum, 3)
  }

  test("read all returns all sent values in order") {
    Async.blocking:
      val channel = QueueChannel[Int]
      channel.send(1)
      channel.send(2)
      channel.send(3)
      channel.close()
      channel.readAll() match
        case Right(Seq(1, 2, 3)) =>
        case _                   => fail("did not get the expected values")
  }

  test("read and read all") {
    Async.blocking:
      val channel = QueueChannel[Int]
      channel.send(1)
      channel.send(2)
      channel.send(3)
      channel.close()
      channel.read() match
        case Right(1) =>
        case _        => fail("expected to read a 1")
      channel.readAll() match
        case Right(Seq(2, 3)) =>
        case _                => fail("did not get the expected values")
  }

  test("read and read all with close") {
    Async.blocking:
      val channel = QueueChannel[Int]
      channel.send(1)
      channel.send(2)
      channel.send(3)
      channel.close()
      channel.read() match
        case Right(1) =>
        case _        => fail("expected to read a 1")
      channel.readAll() match
        case Right(Seq(2, 3)) =>
        case _                => fail("did not get the expected values")
      channel.read() match
        case Left(Closed) =>
        case _            => fail("expected the channel to be closed")
  }
