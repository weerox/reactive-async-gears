package rasync
package handler

import gears.async.Async

import cell.Cell
import util.{ Container, ContainerMap }

private[rasync] trait DependencyHandler[V, Args, Params]
    extends Handler[Params, Outcome[V]]:
  val arguments: Args

  type Dependency <: Cell[?]
  def dependencies: Iterable[Dependency]

private[rasync] class IterableDependencyHandler[V](
    val arguments: Iterable[Cell[V]],
    val handler: Iterable[State[V]] => Async ?=> Outcome[V]
) extends DependencyHandler[V, Iterable[Cell[V]], Iterable[State[V]]]:
  override def run()(using Async): Outcome[V] = handler(arguments.map(cell => cell.state))

  override type Dependency = Cell[V]
  override def dependencies: Iterable[Dependency] = arguments

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

  override type Dependency = Cell[Any]
  override def dependencies: Iterable[Dependency] =
    Iterable.from(arguments.productIterator.asInstanceOf[Iterator[Cell[Any]]])

private[rasync] class SingletonDependencyHandler[V](
    val arguments: Cell[V],
    val handler: State[V] => Async ?=> Outcome[V]
) extends DependencyHandler[V, Cell[V], State[V]]:
  override def run()(using Async): Outcome[V] = handler(arguments.state)

  override type Dependency = Cell[V]
  override def dependencies: Iterable[Dependency] = Iterable.single(arguments)
