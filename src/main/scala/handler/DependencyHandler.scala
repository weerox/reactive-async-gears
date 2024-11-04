package rasync
package handler

import gears.async.Async

import cell.Cell
import util.{ Container, ContainerMap }

private[rasync] trait DependencyHandler[V, Args, Params] extends Handler[Params, Outcome[V]]:
  val arguments: Args

private[rasync] class IterableDependencyHandler[T, V](
    val arguments: Iterable[Cell[T]],
    val handler: Iterable[State[T]] => Async ?=> Outcome[V]
) extends DependencyHandler[V, Iterable[Cell[T]], Iterable[State[T]]]:
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

private[rasync] class SingletonDependencyHandler[T, V](
    val arguments: Cell[T],
    val handler: State[T] => Async ?=> Outcome[V]
) extends DependencyHandler[V, Cell[T], State[T]]:
  override def run()(using Async): Outcome[V] = handler(arguments.state)
