
object Chapter11 {


  /**
   * Functor Laws
   *
   * Identity Rule: fmap id      = id    // if we map id function over a functor we get the orignal functor back
   * Composition Rule: fmap (p . q) = (fmap p) . (fmap q) //composing two functions and then mapping the resulting
   *                                                      //function over a functor
   *                                                      //should be the same as first mapping one function over
   *                                                      //the functor and then mapping the other one.
   *
   *
   * A functor transforms one category into another category.
   *
   * Functor is a generalization of map, they maintain the shape
   * of the container.
   *
   */

  // A type constructor like List or Option or F is a Functor
  // the map function of such a constructor takes the element
  // and returns the element in a container  A => List[A], A => F[A]
  trait Functor[F[_]]{
    def map[A,B](fa:F[A])(f:A=>B):F[B]

    // this works like a generic unzip function
    // for example if F were to be a List then for a list of tuple
    // you get back two lists of the same length
    def distribute[A,B](fab:F[(A,B)]):(F[A],F[B])={
      (map(fab)(_._1),map(fab)(_._2))
    }

    //
    def codistribute[A,B](e:Either[F[A], F[B]]):F[Either[A,B]]={
     e match{
        case Left(fa) =>map(fa)(Left(_))
        case Right(fa) =>map(fa)(Right(_))
     }
    }
  }


  trait Monad[F[_]] extends Functor[F]{
    def unit[A](a: => A):F[A]
    def map[A,B](fa:F[A])(f:A=>B):F[B] ={
      flatMap(fa)(a=>unit(f(a)))
    }
    def flatMap[A,B](fa:F[A])(f:A=>F[B]):F[B]
    def map2[A,B,C](fa:F[A], fb:F[B])(f:(A,B)=>C):F[C]={
      flatMap(fa) (a => map(fb) (b => f(a,b)))
    }

  }

}
