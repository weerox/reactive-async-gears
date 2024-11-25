package rasync
package test

import rasync.cell.CellUpdater
import rasync.util.Dependencies
import rasync.handler.SingletonDependencyHandler
import rasync.lattice.given

import scala.concurrent.duration.*

import gears.async.Async
import gears.async.Future
import gears.async.AsyncOperations
import gears.async.withTimeoutOption
import gears.async.default.given

import util.rerun

class HandlerSourceTests extends util.AsyncSuite:

  val handler = Handler[Int](summon[Lattice[Int]])

  def makeCell() =
    CellUpdater.initial(Complete(handler.lattice.bottom))(using handler)

  def makeInitializedCell() =
    val cell = makeCell()
    cell.update(handler.lattice.bottom)
    cell.complete()
    cell

  def makeDependencyHandler() =
    SingletonDependencyHandler(makeInitializedCell(), makeInitializedCell(), _ => Nothing)

  def makeDependencyHandlerWithUninitializedDependent() =
    SingletonDependencyHandler(makeCell(), makeInitializedCell(), _ => Nothing)

  test("returns all dependency handlers if all dependent cells are initialized".rerun(100)) {
    Async.blocking:
      val deps = Dependencies[Int]
      deps.schedule(Iterable.fill(3)(makeDependencyHandler()))
      val result = deps.awaitResult
      assert(result.isDefined)
      assertEquals(result.get.size, 3)
  }

  test("returns only dependency handlers with initialized dependent cells".rerun(100)) {
    Async.blocking:
      val deps = Dependencies[Int]
      deps.schedule(Iterable.single(makeDependencyHandler()))
      deps.schedule(Iterable.single(makeDependencyHandlerWithUninitializedDependent()))
      deps.schedule(Iterable.single(makeDependencyHandler()))
      val result = deps.awaitResult
      assert(result.isDefined)
      assertEquals(result.get.size, 2)
  }

  test("blocks if all dependency handlers have uninitialized dependent cells".rerun(100)) {
    Async.blocking:
      val deps = Dependencies[Int]
      deps.schedule(Iterable.fill(3)(makeDependencyHandlerWithUninitializedDependent()))
      val result = withTimeoutOption(10.millis):
        deps.awaitResult
      assertEquals(result, None)
  }

  test("non empty stopped dependency source will return handlers".rerun(100)) {
    Async.blocking:
      val deps = Dependencies[Int]
      deps.schedule(Iterable(makeDependencyHandler(), makeDependencyHandler()))
      deps.stop()
      val result = deps.awaitResult
      assert(result.isDefined)
      assertEquals(result.get.size, 2)
  }

  test("empty stopped dependency source will immediately return".rerun(100)) {
    Async.blocking:
      val deps = Dependencies[Int]
      deps.stop()
      val result = deps.awaitResult
      assertEquals(result, None)
  }

  test("two listeners will make one wait indefinitely".rerun(100)) {
    Async.blocking:
      val deps = Dependencies[Int]
      val a = Future:
        deps.awaitResult
      val b = Future:
        AsyncOperations.sleep(10)
        deps.awaitResult
      val dep = makeDependencyHandler()
      deps.schedule(Iterable(dep))
      val x = a.await
      val y = withTimeoutOption(20.millis):
        b.await
      assertEquals(x, Some(Iterable(dep)))
      assertEquals(y, None)
  }
