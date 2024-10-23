package rasync

class ReactiveAsyncTests extends munit.FunSuite {
  class NumberLattice extends Lattice[Int]:
    override val bottom: Int = 0
    override def join(x: Int, y: Int): Int = x + y
    override def lteq(x: Int, y: Int): Boolean = x <= y

  test("cell initialization") {
    given Lattice[Int] = NumberLattice()
    val (cell1, cell2) = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.cell: () =>
        Update(42)
      val cell2 = ReactiveAsync.cell: () =>
        Update(26)
      (cell1, cell2)
    assertEquals(cell1.get, 42)
    assertEquals(cell2.get, 26)
  }
}
