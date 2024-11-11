import rasync.ReactiveAsync
import rasync.Lattice
import rasync.lattice.given Lattice[Int]
import rasync.{ Nothing, Update }
import rasync.Value

import org.scalameter.*
import org.scalameter.picklers.Implicits.*
object GraphBench extends Bench.LocalTime:
  val n     = Gen.single("nodes")(10000)
  val p     = n.map(n => ((1 + 0) * math.log(n)) / n)
  val graph = n.cross(p)

  /*
  This was measured for a version of the benchmark with a bug that removed the dependencies whenever you updated the cell.
  This means that each dependency handler only ran once. The same graph benchmark for rasync2 ran each handler, on average, 1.5 times.
  [a59925]  n = 10000, p = ln / n -> 755 ms
  [rasync2] n = 10000, p = ln / n -> 737 ms (16 threads)
  [a59925]  n = 20000, p = ln / n -> 2854 ms
  [rasync2] n = 20000, p = ln / n -> 2842 ms (16 threads)

  [d18ecf] These are the measurements after the bug was fixed.
           We also now keep track of what cells were updated so we only have to run the relevant
           handlers (instead of before, where all handlers basically ran all the time).
           This graph benchmark will now generate 2 times more dependency calls compared to rasync2.

  [036003] This will additionally keep a mapping from cells to what handlers they are a dependency for,
           so it is quick to know exactly what handlers should be scheduled.
           When a handler detects that all the dependencies have entered a state where it cannot be
           changed anymore, it will tell the dependent to remove the dependency handler from itself.
           As with [d18ecf] this benchmark will generate 2 times more dependency calls compared to rasync2.

  [d18ecf]  n = 10000, p = ln n / n ->  940 ms
  [036003]  n = 10000, p = ln n / n ->  988 ms
  [rasync2] n = 10000, p = ln n / n ->  760 ms (16 threads)
  [d18ecf]  n = 20000, p = ln n / n -> 3363 ms
  [036003]  n = 20000, p = ln n / n -> 3405 ms
  [rasync2] n = 20000, p = ln n / n -> 2862 ms (16 threads)

  I realised that the majority of the time for my implemenation was spent creating the cells and dependencies rather than actually executing the dependencies.
  rasync2 has the advantage that it starts executing before all cells and dependencies have been created.
  The probability is calculated from epsilon using p = (1 + epsilon) * ln(n) / n
  Below are a couple of runs where I've increased the probability to generate an edge.
  Intrestingly, the CPU utilization is similar between both of them for each measurment configuration.
  None of these will utilize all 16 threads on my computer. I didn't see them go over 300 % utilization.

  n = 10000, epsilon = 0.0 -> 92 010 edges,  [036003] 988 ms,  [rasync2] 762 ms
  n = 10000, epsilon = 0.1 -> 101 269 edges, [036003] 1017 ms, [rasync2] 792 ms
  n = 10000, epsilon = 0.2 -> 110 644 edges, [036003] 1060 ms, [rasync2] 778 ms
  n = 10000, epsilon = 0.4 -> 128 816 edges, [036003] 1104 ms, [rasync2] 802 ms
  n = 10000, epsilon = 0.8 -> 166 045 edges, [036003] 1252 ms, [rasync2] 835 ms
  n = 10000, epsilon = 1.5 -> 230 698 edges, [036003] 1420 ms, [rasync2] 903 ms
  n = 10000, epsilon = 2.0 -> 276 541 edges, [036003] 1531 ms, [rasync2] 954 ms

  I also measured the time to create 10000 nodes/cells and their edges/dependencies with epsilon = 2
  without actually executing the dependencies.
  [036003]  -> 802 ms
  [rasync2] -> 827 ms
  If we remove the time it takes to set up the problem from the new implementation,
  we get that 1531 ms - 802 ms = 729 ms is spent executing the dependency handlers.
  If the execution could be started in tandem with the problem creation, then we should probably end up
  slightly below the time it takes for rasync2 to solve the whole problem.
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
