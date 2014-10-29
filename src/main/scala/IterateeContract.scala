
object IterateeContract {
  // Enumerator is a producer of the data
  // Iteratee is the consumer of the data
  // Enumeratee is the transformer that takes the data of one format from the enumerator
  // and converts it to another format.

  // Producer produces the data and sends it to the Consumer when the data is available
  // this is more scalable then consumer pulling data as the consumer would be blocked
  // when data is not available

  /*

  // Consumer
  trait Iteratee[E,+A]{
    def fold[B](folder:Step[E,A] => B):B
    def run:A
    def flatMap[B](f:A => Iteratee[E,B]):Iteratee[E,B]
    def map[B](f:A=>B):Iteratee[E,B]
  }

  // Producer
  trait Enumerator[E]{
    parent =>
    def apply[A](e:Iteratee[E,A]):Iteratee[E,A]
  }

  //Transformer
  trait Enumeratee[From,To]{
    def apply[A](e:Iteratee[To,A]):Iteratee[From,A]
  }



  // Inputs to the consumer or Iteratee
  sealed trait Input[+E]{
    def map[U](f:E =>U):Input[U]= this match{
      case El(e) => El(f(e))
      case EOF => EOF
      case Empty => Empty
    }
  }
  case class  El[+E](e:E) extends Input[E]
  case object EOF extends Input[Nothing]
  case object Empty extends Input[Nothing]




  def counter[E]: Iteratee[E, Int] = {
    def step(count: Int): Input[E] => Iteratee[E, Int] = {
      case El(e) => Cont(step(count + 1))
      case EOF => Done(count, EOF)
      case Empty => Cont(step(count))
    }
    Cont(step(0))
  }
                    */

}
