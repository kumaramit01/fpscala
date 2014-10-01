object Chapter3 extends App{


  sealed trait List[+A] // `List` data type, parameterized on a type, `A`
  case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
  case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

  object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //EX 3.2
    def tail[A](list:List[A]):List[A]={
      list match{
        case Nil => Nil
        case Cons(_,tail)=>tail
      }
    }

    //EX 3.3
    def setHead[A](y:A,list:List[A]):List[A]={
      list match{
        case Nil => Cons(y,Nil)
        case Cons(_,tail)=>Cons(y,tail)
      }
    }

    //EX 3.4
    def drop[A](list:List[A],n:Int):List[A]={
      if(n==0){
       list
      }else{
        list match{
          case Nil => Nil
          case Cons(_,tail)=> drop(tail,n-1)
        }
      }
    }

    // EX 3.5
    def dropWhile[A](list:List[A], f:(A=>Boolean)):List[A]={
      list match{
        case Nil => Nil
        case Cons(head@_,tail) if(f(head)) => dropWhile(tail,f)
        case _ => list
      }
    }

    //EX 3.6
    def init[A](list:List[A]):List[A]={
      def _init(l:List[A],result:List[A]):List[A]={
        l match{
          case Cons(_, Nil)=>result
          case Cons(head,tail) =>Cons(head,_init(tail,result))
        }

      }
     _init(list,Nil)
    }


   }


  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }


 require(List.tail(List(1,2,3,4,5)) == List(2,3,4,5))
 require(List.setHead(10,List(1,2,3,4,5)) == List(10,2,3,4,5))
 require(List.drop(List(1,2,3,4,5),3) == List(4,5))
 require(List.drop(List(1,2,3,4,5),1) == List(2,3,4,5))
 require(List.drop(List(1,2,3,4,5),5) == Nil)
 require(List.drop(List(1,2,3,4,5),6) == Nil)

 require(List.dropWhile(List(1,2,3,4,5), (x:Int)  => x < 3) == List(3,4,5))
 require(List.dropWhile(List(1,2,3,4,5), (x:Int)  => x < 10) == Nil)
 require(List.dropWhile(List(1,2,3,4,5), (x:Int)  => x < 1) == List(1,2,3,4,5))


  require(List.init(List(1,2,3,4,5)) == List(1,2,3,4))
  require(List.init(List(1)) == Nil)



}
