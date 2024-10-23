package rasync

class CellUpdaterTests extends munit.FunSuite {

  class NumberLattice extends Lattice[Int]:
    override val bottom: Int = 0
    override def join(x: Int, y: Int): Int = x + y
    override def lteq(x: Int, y: Int): Boolean = x <= y

  val handler = Handler[Int](NumberLattice())

  test("cell update") {
    val cell = CellUpdater[Int](using handler)
    cell.update(1)
    assertEquals(cell.get, 1)
  }
}
