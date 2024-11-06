package rasync
package test

import lattice.given

class ReactiveAsyncTests extends munit.FunSuite:
  test("cell initialization") {
    val (cell1, cell2) = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.cell: () =>
        Update(42)
      val cell2 = ReactiveAsync.cell: () =>
        Update(26)
      (cell1, cell2)
    assertEquals(cell1.get, 42)
    assertEquals(cell2.get, 26)
  }

  test("one length path cell dependency") {
    val cell = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.cell: () =>
        Complete(Some(72))
      val cell2 = ReactiveAsync.cell

      cell2.when(cell1)(cell =>
        cell match
          case Completed(value) => Complete(Some(value))
          case _                => Nothing
      )

      cell2
    assertEquals(cell.get, 72)
  }

  test("two length path cell dependency") {
    val cell = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.cell: () =>
        Complete(Some(72))
      val cell2 = ReactiveAsync.cell
      val cell3 = ReactiveAsync.cell

      cell2.when(cell1)(cell =>
        cell match
          case Completed(value) => Complete(Some(value))
          case _                => Nothing
      )
      cell3.when(cell2)(cell =>
        cell match
          case Completed(value) => Complete(Some(value))
          case _                => Nothing
      )

      cell3
    assertEquals(cell.get, 72)
  }

  test("exception in initializer results in a failed state") {
    val cell = ReactiveAsync.handler:
      val cell = ReactiveAsync.cell[Int](() => throw Exception())
      cell

    cell.state match
      case Failed(_) =>
      case _         => fail("state was not Failed", clues(cell.state))
  }

  test("exception in dependency handler results in a failed state") {
    val cell = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.cell[Int]
      val cell2 = ReactiveAsync.cell[Int]

      cell2.when(cell1)(cell => throw Exception())
      cell2

    cell.state match
      case Failed(_) =>
      case _         => fail("state was not Failed", clues(cell.state))
  }

  test("get throws when cell is in failed state") {
    val cell = ReactiveAsync.handler:
      val cell = ReactiveAsync.cell[Int](() => throw Exception())
      cell

    intercept[Exception] {
      cell.get
    }
  }
