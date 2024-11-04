package rasync
package test

import cell.CellUpdater
import lattice.given

class CellUpdaterTests extends munit.FunSuite:
  val handler = Handler[Int](summon[Lattice[Int]])

  test("initial cell value") {
    val cell = CellUpdater(using handler)
    assertEquals(cell.get, handler.lattice.bottom)
  }

  test("single cell update") {
    val cell = CellUpdater(using handler)
    cell.update(1)
    assertEquals(cell.get, 1)
  }

  test("two cell updates") {
    val cell = CellUpdater(using handler)
    cell.update(1)
    cell.update(2)
    assertEquals(cell.get, 2)
  }

  test("multiple cell updates") {
    import scala.util.Random.shuffle
    val cell = CellUpdater(using handler)
    for i <- shuffle(1 to 10) do cell.update(i)
    assertEquals(cell.get, 10)
  }
