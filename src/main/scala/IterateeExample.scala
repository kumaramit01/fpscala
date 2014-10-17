
/**
 * An example implementation of Iteratee/Enumerator and Enumeratee
 */
object IterateeExample {

  import IterateeContract.Step._
  import IterateeContract._

  // implement a very simple iteratee called counter that returns the length of
  // the list
 /*
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
