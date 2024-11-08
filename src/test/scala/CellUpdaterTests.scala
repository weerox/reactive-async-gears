package rasync
package test

import cell.CellUpdater
import lattice.given

class CellUpdaterTests extends munit.FunSuite:

  val handler = Handler[Int](summon[Lattice[Int]])

  test("initially uninitialized") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.state match
      case Uninitialized() =>
      case _               => fail("state was not Uninitialized", clues(cell.state))
  }

  test("uninitialized cell value") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    intercept[Exception] {
      cell.get
    }
  }

  test("updating uninitialized cell makes it intermediate") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.update(42)
    cell.state match
      case Intermediate(_) =>
      case _               => fail("state was not Intermediate", clues(cell.state))
  }

  test("completing uninitialized cell throws exception") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    intercept[Exception] {
      cell.complete()
    }
  }

  test("single cell update") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.update(1)
    assertEquals(cell.get, 1)
  }

  test("two cell updates") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.update(1)
    cell.update(2)
    assertEquals(cell.get, 2)
  }

  test("multiple cell updates") {
    import scala.util.Random.shuffle
    val cell = CellUpdater.initial(Nothing)(using handler)
    for i <- shuffle(1 to 10) do cell.update(i)
    assertEquals(cell.get, 10)
  }

  test("cell failure updates state") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.fail(Exception())
    cell.state match
      case Failed(_) =>
      case _         => fail("state was not Failed", clues(cell.state))
  }

  test("get throws when failed") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.fail(Exception())
    intercept[Exception] {
      cell.get
    }
  }

  test("completed cell will not become failed") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.update(42)
    cell.complete()
    cell.fail(Exception())

    cell.state match
      case Failed(_) => fail("state became Failed", clues(cell.state))
      case _         =>
  }

  test("keep first failed state") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.fail(Exception("foo"))
    cell.fail(Exception("bar"))

    cell.state match
      case Failed(e) => assertEquals(e.getMessage(), "foo")
      case _         => fail("state was not Failed", clues(cell.state))
  }

  test("has value") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.update(42)
    assertEquals(cell.hasValue(), true)
    cell.complete()
    assertEquals(cell.hasValue(), true)
  }

  test("is completed") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.update(42)
    cell.complete()
    assertEquals(cell.isCompleted(), true)
  }

  test("is failed") {
    val cell = CellUpdater.initial(Nothing)(using handler)
    cell.fail(Exception())
    assertEquals(cell.isFailed(), true)
  }
