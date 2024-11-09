package rasync
package test

import lattice.given

class ReactiveAsyncTests extends munit.FunSuite:
  test("create cell using default initial value") {
    val cell = ReactiveAsync.handler:
      ReactiveAsync.cell[Int]
    assertEquals(cell.get, summon[Lattice[Int]].bottom)
  }

  test("create cell with initializer") {
    val cell = ReactiveAsync.handler:
      ReactiveAsync.initialize:
        val x = 42
        val y = 54
        Complete(x + y)
    assertEquals(cell.get, 96)
  }

  test("create cell completed with given initial value") {
    val cell = ReactiveAsync.handler:
      ReactiveAsync.completed(42)
    assertEquals(cell.get, 42)
  }

  test("create cell with initial value given outcome") {
    val cell = ReactiveAsync.handler:
      ReactiveAsync.initial(Complete(42))
    assertEquals(cell.get, 42)
  }

  test("one length path cell dependency") {
    val cell = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.completed(72)
      val cell2 = ReactiveAsync.cell

      cell2.when(cell1)(cell =>
        cell match
          case Completed(value) => Complete(value)
          case _                => Nothing
      )

      cell2
    assertEquals(cell.get, 72)
  }

  test("two length path cell dependency") {
    val cell = ReactiveAsync.handler:
      val cell1 = ReactiveAsync.completed(72)
      val cell2 = ReactiveAsync.cell
      val cell3 = ReactiveAsync.cell

      cell2.when(cell1)(cell =>
        cell match
          case Completed(value) => Complete(value)
          case _                => Nothing
      )
      cell3.when(cell2)(cell =>
        cell match
          case Completed(value) => Complete(value)
          case _                => Nothing
      )

      cell3
    assertEquals(cell.get, 72)
  }

  test("exception in initializer results in a failed state") {
    val cell = ReactiveAsync.handler:
      val cell = ReactiveAsync.initialize[Int]:
        throw Exception()
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
      val cell = ReactiveAsync.initialize[Int]:
        throw Exception()
      cell

    intercept[Exception] {
      cell.get
    }
  }

  test("sleep in initializer") {
    import gears.async.AsyncOperations
    import gears.async.default.given AsyncOperations
    val cell = ReactiveAsync.handler:
      ReactiveAsync.initialize:
        AsyncOperations.sleep(100)
        Complete(42)

    assertEquals(cell.get, 42)
  }
