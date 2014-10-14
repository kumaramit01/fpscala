
/**
 * An example implementation of Iteratee/Enumerator and Enumeratee
 */
object IterateeExample {

  import IterateeContract.Step._
  import IterateeContract._

  // implement a very simple iteratee called counter that returns the length of
  // the list

 // def counter[E]:Iteratee[E,Int]={



      /*def step(count:Int):Input[E] => Iteratee[E,Int]={
        case El(e) => Cont(step(count + 1))
        case Empty => Cont(step(count))
        case EOF => Done(count,EOF)

      }
      Cont(step(0))
       */
    //def fold[B](folder: (Step[E, Int]) => B): B =

      //def run: Int = ???

    //def flatMap[B](f: (Int) => Iteratee[E, B]): Iteratee[E, B] = ???

    //def map[B](f: (Int) => B): Iteratee[E, B] = ???
  //}




}
