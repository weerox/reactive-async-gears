package rasync
package bench

import org.scalameter.*
import org.scalameter.picklers.Implicits.*

class SetLattice[T] extends Lattice[Set[T]]:
  override val bottom: Set[T] = Set()

  override def join(x: Set[T], y: Set[T]): Set[T]  = x union y
  override def lteq(x: Set[T], y: Set[T]): Boolean = x subsetOf y

class Marker

object TreeBench extends Bench.LocalTime:
  val depth  = Gen.single("depth")(14)
  val degree = Gen.single("degree")(2)
  val tree   = depth.cross(degree)

  /*
  2^16 leaves:
  [c0fa29] Using numbers with lattice sum: 238 ms
  [c0fa29] Using empty class and set lattice with union: 390 ms
  [c0fa29] Using empty class and set lattice with union and only fold when all complete: 370 ms

  The previous RA library, with 2^13 leaves: 413 ms
  The previous RA library, with 2^14 leaves: 1794 ms
  [c0fa29] This implementation, with 2^14 leaves: 76 ms
   */

  performance of "Tree" in {
    using(tree) in { (depth, degree) =>
      def build(height: Int, degree: Int)(using
          Handler[Set[Marker]]
      ): Cell[Set[Marker]] =
        if height == 0 then
          ReactiveAsync.cell(() => Complete(Some(Set(Marker()))))
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
              Complete(Some(set))
            else Nothing
          }
          sum

      given Lattice[Set[Marker]] = SetLattice()
      val result = ReactiveAsync.handler:
        build(depth, degree)

      assert(result.get.size == math.pow(degree, depth))
    }
  }
