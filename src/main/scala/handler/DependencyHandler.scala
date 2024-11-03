package rasync
package handler

import gears.async.Async

import cell.Cell
import util.{ Container, ContainerMap }

private[rasync] trait DependencyHandler[V, Args, Params] extends Handler[Params, Outcome[V]]:
  val arguments: Args

private[rasync] class IterableDependencyHandler[V](
    val arguments: Iterable[Cell[V]],
    val handler: Iterable[State[V]] => Async ?=> Outcome[V]
) extends DependencyHandler[V, Iterable[Cell[V]], Iterable[State[V]]]:
  override def run()(using Async): Outcome[V] = handler(arguments.map(cell => cell.state))

private[rasync] class TupleDependencyHandler[
    V,
    Args <: Tuple: Container[Cell],
    Params <: ContainerMap[Args, Cell, State]
](
    val arguments: Args,
    val handler: Params => Async ?=> Outcome[V]
) extends DependencyHandler[V, Args, Params]:
  override def run()(using Async): Outcome[V] =
    import scala.runtime.Tuples.fromIArray
    val array = arguments.productIterator
      .map[State[?]](e => e.asInstanceOf[Cell[?]].state)
      .toArray
      .asInstanceOf[IArray[Object]]
    val args = fromIArray(array).asInstanceOf[Params]
    handler(args)

private[rasync] class SingletonDependencyHandler[V](
    val arguments: Cell[V],
    val handler: State[V] => Async ?=> Outcome[V]
) extends DependencyHandler[V, Cell[V], State[V]]:
  override def run()(using Async): Outcome[V] = handler(arguments.state)
