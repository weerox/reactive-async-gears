package rasync.test.util

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

abstract class AsyncSuite extends munit.FunSuite:
  given ExecutionContext = ExecutionContext.global

  override def munitTestTransforms = super.munitTestTransforms ++ List(
    new TestTransform(
      "Rerun",
      { test =>
        val rerunCount = test.tags
          .collectFirst { case Rerun(n) => n }
          .getOrElse(1)
        if rerunCount == 1 then test
        else
          test.withBody { () =>
            Future.sequence(1.to(rerunCount).map(_ => test.body()).toList)
          }
      }
    )
  )
