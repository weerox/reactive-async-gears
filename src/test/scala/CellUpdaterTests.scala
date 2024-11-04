package rasync
package test

import cell.CellUpdater

class CellUpdaterTests extends munit.FunSuite:

  class NumberLattice extends Lattice[Int]:
    override val bottom: Int                   = 0
    override def join(x: Int, y: Int): Int     = x + y
    override def lteq(x: Int, y: Int): Boolean = x <= y

  val handler = Handler()
  val lattice = NumberLattice()

  test("initial cell value") {
    val cell = CellUpdater(using handler, lattice)
    assertEquals(cell.get, lattice.bottom)
  }

  test("single cell update") {
    val cell = CellUpdater(using handler, lattice)
    cell.update(1)
    assertEquals(cell.get, 1)
  }

  test("two cell updates") {
    val cell = CellUpdater(using handler, lattice)
    cell.update(1)
    cell.update(1)
    assertEquals(cell.get, 2)
  }

  test("multiple cell updates") {
    val cell = CellUpdater(using handler, lattice)
    for _ <- 1 to 10 do cell.update(1)
    assertEquals(cell.get, 10)
  }
