import _root_.IterateeContract.{Input, Iteratee}

/**
 * Created with IntelliJ IDEA.
 * User: akumar2
 * Date: 10/26/14
 * Time: 5:45 PM
 * To change this template use File | Settings | File Templates.
 */
// States of the consumer
sealed trait Step[E, +A]

object Step{
  case class Done[E,+A](state:A, remaining:Input[E]) extends Step[E,A]  with Iteratee[E,A]{
    def fold[B](folder: (Step[E, A]) => B): B = folder(this)

    def run: A = state

    def flatMap[B](f: (A) => Iteratee[E, B]): Iteratee[E, B] = ???

    def map[B](f: (A) => B): Iteratee[E, B] = ???
  }
  case class Cont[E,+A](k:Input[E] => Iteratee[E,A]) extends Step[E,A]  with Iteratee[E,A]{
    def fold[B](folder: (Step[E, A]) => B): B = ???

    def run: A = ???

    def flatMap[B](f: (A) => Iteratee[E, B]): Iteratee[E, B] = ???

    def map[B](f: (A) => B): Iteratee[E, B] = ???
  }
  case class Error[E,+A](t:Throwable) extends Step[E,A] with Iteratee[E,A]{
    def fold[B](folder: (Step[E, A]) => B): B = ???

    def run: A = ???

    def flatMap[B](f: (A) => Iteratee[E, B]): Iteratee[E, B] = ???

    def map[B](f: (A) => B): Iteratee[E, B] = ???
  }
}