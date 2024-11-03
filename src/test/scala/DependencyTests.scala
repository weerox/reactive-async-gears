package rasync
package test

class DependencyTests extends munit.FunSuite:

  class NumberLattice extends Lattice[Int]:
    override val bottom: Int                   = 0
    override def join(x: Int, y: Int): Int     = x.max(y)
    override def lteq(x: Int, y: Int): Boolean = x <= y

  given Lattice[Int] = NumberLattice()

  test("iterable has same size") {
    val n = 5
    ReactiveAsync.handler:
      val cell  = ReactiveAsync.cell[Int]
      val cells = Seq.fill(n)(ReactiveAsync.cell[Int])

      cell.when(cells)(list =>
        assertEquals(cells.size, list.size)
        Complete(Some(0))
      )
  }
