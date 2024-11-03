package rasync
package handler

import gears.async.Async

private[rasync] class InitializationHandler[V](
    val init: () => Async ?=> Outcome[V]
) extends Handler[Unit, Outcome[V]]:
  // We can't use "empty parameter list" as a type when extending `Handler`,
  // so instead we specified `Unit`, but we don't want to force developers to
  // take `Unit` as a parameter when creating `Cell`s.
  // So the constructor for this handler will take a function with an empty
  // parameter list here we create a handler that takes a `Unit` and runs the
  // parameter-less function given in the constructor.
  val handler: Unit => (Async) ?=> Outcome[V] = Unit => init()
  def run()(using Async): Outcome[V]          = handler(())
