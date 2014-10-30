package reactive

import scala.util.control.NonFatal

/**
 * Data structures with map and flatmap togather with some algebraic laws
 */
object Monads {

  // Associativity( m flatmap) f flatmap g == m
  // Left unit unit(x) flatMap f = f(x)
  // Right unit m flatMap unit =   m

  trait M[T]{
    // called bind
    def flatMap[U](f:T =>M[U]):M[U]
    def unit[T](x:T):M[T]
    def map[U](f:T=>U):M[U]= flatMap(f andThen unit)
  }

  /*
  abstract class Option[+T]{

    def flatMap[U](f:T=>Option[U]):Option[U]={
      this match {
        case None => None
        case Some(x) => f(x)
      }
    }
  }
   */

  // significance of the law

  // Associative law allows you to inline the for generator
  // Right unit law says for(x <- m) yield x == m


 /*
  abstract class Try[+T]
  case class Success[T](x:T) extends Try[T]
  case class Failure(ex:Exception) extends Try[Nothing]

  object Try{
    def apply[T](expr: => T):Try[T]=
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
  }

  for {
  x <- computeX
  y <- computeY
               } yield f(x,y)



  abstract class Try[T]{
    def flatMap[U](f:T => Try[U]):Try[U]= this match{
      case Success(x) => try f(x) catch {case NonFatal(ex) => Failure(ex)}
      case fail:Failure => fail
    }
    def map[U](f:T=>U):Try[U]= this match {
      case Success(x) => Try(f(x))
      case fail:Failure => fail
    }

  }
  // the map can be written as flatMap
  t map f == t flatMap (x => Try(f(x)))
          == t flatMap (f andThen Try)

   */


}
