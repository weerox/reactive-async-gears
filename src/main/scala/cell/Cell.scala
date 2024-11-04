package rasync
package cell

import gears.async.Async

import util.{ Container, ContainerMap }

trait Cell[V]:
  def get: V
  def state: State[V]
  def isComplete(): Boolean

  def when[T](dependencies: Iterable[Cell[T]])(
      body: Iterable[State[T]] => Async ?=> Outcome[V]
  ): Unit

  def when[T](dependencies: Cell[T])(
      body: State[T] => Async ?=> Outcome[V]
  ): Unit

  def when[
      Args <: Tuple: Container[Cell],
      Params <: ContainerMap[Args, Cell, State]
  ](dependencies: Args)(
      body: Params => Async ?=> Outcome[V]
  ): Unit
