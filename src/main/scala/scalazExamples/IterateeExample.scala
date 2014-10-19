package scalazExamples

import scalaz._


object IterateeExample {

  import scalaz._, Scalaz._, MonadPartialOrder._
  import iteratee._, Iteratee._
  import effect._


  val stream123 = enumStream[Int, Id](Stream(1, 2, 3))

  ((head[Int, Id]   &= stream123).run) assert_=== Some(1)
  ((length[Int, Id] &= stream123).run) assert_=== 3
  ((peek[Int, Id]   &= stream123).run) assert_=== Some(1)
  ((head[Int, Id]   &= enumStream(Stream())).run) assert_=== None

}
