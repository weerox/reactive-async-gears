import rasync.{ Handler, ReactiveAsync }
import rasync.{ Completed, Value }
import rasync.{ Complete, Nothing, Update }
import rasync.cell.Cell
import rasync.lattice.given

import org.scalameter.*
import org.scalameter.picklers.Implicits.*

class Marker

object TreeBench extends Bench.LocalTime:
  val depth  = Gen.single("depth")(16)
  val degree = Gen.single("degree")(2)
  val tree   = depth.cross(degree)

  /*
  [rasync2] 2^13 -> 413 ms
  [rasync2] 2^14 -> 1794 ms
  [32a8d4]  2^14 -> 76 ms
  [32a8d4]  2^16 -> 399 ms
  [2100d9]  2^16 -> 442 ms
   */

  /** Constructs a tree of given `depth` and `degree`. */
  performance of "Tree" in {
    using(tree) in { (depth, degree) =>
      def build(height: Int, degree: Int)(using
          Handler[Set[Marker]]
      ): Cell[Set[Marker]] =
        if height == 0 then
          ReactiveAsync.completed(Set(Marker()))
        else
          val cells = Seq.fill(degree)(build(height - 1, degree))
          val sum   = ReactiveAsync.cell[Set[Marker]]
          sum.when(cells) { cells =>
            if cells.forall(cell =>
                cell match
                  case Completed(_) => true
                  case _            => false
              )
            then
              val set: Set[Marker] = cells
                .foldLeft(Set()) { (acc, x) =>
                  x match
                    case Completed(x) => acc union x
                    case _            => acc
                }
              Complete(set)
            else Nothing
          }
          sum

      val result = ReactiveAsync.handler:
        build(depth, degree)

      assert(result.get.size == math.pow(degree, depth))
    }
  }

  /*
  [2100d9] 2^16 -> 219 ms
   */

  /** Constructs a tree of given `depth` and a degree of 2. */
  performance of "Binary Tree" in {
    using(depth) in { depth =>
      def build(height: Int)(using Handler[Int]): Cell[Int] =
        if height == 0 then ReactiveAsync.cell
        else
          val left  = build(height - 1)
          val right = build(height - 1)
          val sum   = ReactiveAsync.cell
          sum.when(left)(left =>
            left match
              case Value(value) => Update(value)
              case _            => Nothing
          )
          sum.when(right)(right =>
            right match
              case Value(value) => Update(value)
              case _            => Nothing
          )
          sum

      ReactiveAsync.handler:
        build(depth)
    }
  }
