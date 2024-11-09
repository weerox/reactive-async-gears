package rasync
package test

import lattice.given

class DependencyTests extends munit.FunSuite:
  test("iterable has same size") {
    val n = 5
    ReactiveAsync.handler:
      val cell  = ReactiveAsync.cell[Int]
      val cells = Seq.fill(n)(ReactiveAsync.cell[Int])

      cell.when(cells)(list =>
        assertEquals(cells.size, list.size)
        Complete(0)
      )
  }
