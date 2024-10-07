import gears.async.*
import gears.async.default.given

@main def hello() =
  Async.blocking:
    val hello = Future:
      print("Hello")
    val world = Future:
      hello.await
      println(", world!")
    world.await
