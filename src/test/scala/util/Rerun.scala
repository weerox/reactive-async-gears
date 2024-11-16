package rasync.test.util

/** Rerun the test `count` times.
  *
  * @example
  *   {{{
  * test("foo".tag(Rerun(50))) {
  *   ???
  * }
  *   }}}
  */
case class Rerun(count: Int) extends munit.Tag("Rerun")

/** Rerun the test `count` times.
  *
  * @example
  *   {{{
  * test("foo".rerun(50)) {
  *   ???
  * }
  *   }}}
  */
extension (options: munit.TestOptions)
  def rerun(count: Int) = options.tag(Rerun(count))
