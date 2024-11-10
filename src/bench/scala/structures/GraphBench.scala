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
  [2100d9] n = 10000, p = ln / n -> 755 ms
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
        val nodes = List.fill(n)(ReactiveAsync.cell[Int])
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
