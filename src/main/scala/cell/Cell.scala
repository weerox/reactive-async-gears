package rasync
package cell

import gears.async.Async

import util.{ Container, ContainerMap }

trait Cell[V]:
  def get: V
  def state: State[V]

  def isCompleted(): Boolean
  def isFailed(): Boolean
  def hasValue(): Boolean

  def when(dependencies: Iterable[Cell[V]])(
      body: Iterable[State[V]] => Async ?=> Outcome[V]
  ): Unit

  def when(dependencies: Cell[V])(
      body: State[V] => Async ?=> Outcome[V]
  ): Unit

  def when[
      Args <: Tuple: Container[Cell],
      Params <: ContainerMap[Args, Cell, State]
  ](dependencies: Args)(
      body: Params => Async ?=> Outcome[V]
  ): Unit
