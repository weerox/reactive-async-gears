import rasync.ReactiveAsync
import rasync.Lattice
import rasync.lattice.given Lattice[Int]
import rasync.{ Nothing, Update }
import rasync.Value

import org.scalameter.*
import org.scalameter.picklers.Implicits.*
object GraphBench extends Bench.LocalTime:
  val n     = Gen.single("nodes")(10000)
  val p     = n.map(n => math.log(n) / n)
  val graph = n.cross(p)

  /*
  This was measured for a version of the benchmark with a bug that removed the dependencies whenever you updated the cell.
  This means that each dependency handler only ran once. The same graph benchmark ran each handler, on average, 1.5 times.
  [a59925]  n = 10000, p = ln / n -> 755 ms
  [rasync2] n = 10000, p = ln / n -> 737 ms (16 threads)
  [a59925]  n = 20000, p = ln / n -> 2854 ms
  [rasync2] n = 20000, p = ln / n -> 2842 ms (16 threads)
   */

  /** Constructs a graph with `n` nodes and a probability `p` of any two nodes being connected.
    *
    * The graph generation is based on the
    * [[https://en.wikipedia.org/wiki/Erd%C5%91s%E2%80%93R%C3%A9nyi_model Erdős–Rényi G(n, p) model]],
    * where p = ln n / n is a "sharp threshold for the connectedness of G(n, p)".
    */
  performance of "Graph" in {
    using(graph) in { (n, p) =>
      val random = util.Random(20001026)
      ReactiveAsync.handler:
        val nodes = List.fill(n)(ReactiveAsync.initial[Int](Update(util.Random.between(0, 99))))
        for
          i <- nodes
          j <- nodes
        do
          if random.nextDouble() < p then
            i.when(j)(cell =>
              cell match
                case Value(value) => Update(value)
                case _            => Nothing
            )
    }
  }
