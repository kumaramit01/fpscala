import scala.annotation.tailrec
import scala.Function

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


    // fold right implementation
    def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B):B = as match{
      case Nil => z
      case Cons(head,tail) =>  f(head,foldRight(tail,z)(f))
    }

    // fold right implementation breaks when 0 is present -but still returns
    // what ever the accumalator has at that moment
    def breakAblefoldRight(as:List[Int],z:Int)(f:(Int,Int)=>Int):Int ={
      var breakEarly=false   // <- this doesn't work either... we still get what ever accumalator has
      as match{
        case Nil => if(!breakEarly) z else 0
        case Cons(head,tail) =>  if(head == 0){  breakEarly = true; return 0; } else {f(head,breakAblefoldRight(tail,z)(f)) }
      }

    }
     // Ex 3.9
    def length[A](as:List[A]):Int={
      foldRight[A,Int](as,0:Int){ (a,b) => b + 1 }
    }



    @tailrec
    def foldLeft[A,B](as:List[A], z:B)(f:(B,A)=>B):B=
    as match{
        case Nil => z
        case Cons(head,tail) => foldLeft(tail, f( z , head ) )(f)
    }


    // 3.12
    def reverse[A](as:List[A]):List[A]={
      @tailrec
      def _reverse(list:List[A], result:List[A]):List[A]={
        list match{
          case Nil => result
          case Cons(head,tail) =>  _reverse(tail,Cons(head,result))
        }
      }
      _reverse(as,Nil)
    }

    // 3.12
    def reverseUsingFold[A](as:List[A]):List[A]={
      foldLeft(as, Nil:List[A]){case (a,b) => Cons(b,a)}
    }

    // 3.13
    // foldRight via foldLeft
    def foldRightUsingFoldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B={
      foldLeft(reverse(as),z)(f)
    }



    def appendUsingFold[A](as:List[A], element:A):List[A]={
       foldRight(as,Cons(element,Nil:List[A]))(Cons(_,_))
    }

    def appendUsingFold[A](as:List[A], element:List[A]):List[A]={
      foldRight(as,element)(Cons(_,_))
    }

    // concatenate List[List[A]] -> List[A]
    def concatenate[A](as:List[List[A]]):List[A]={
      foldRight(as,Nil:List[A])(appendUsingFold)
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


  val x1= List.foldRight(List(1,2,3,4),0)(_+_)
  val x2= List.foldRight(List(1,2,3,4),1)(_*_)

  println(" ===> "+ x1 + " " + x2)
  // Exercise 3.7
  val x3 = List.breakAblefoldRight(List(1,2,0,3,4),1)(_+_)
  println("X3 is: "+ x3)    // 1+2 =3

  // Exercise 3.8
  // folds replace the Nil of the list with z (default value) and cons with
  // the function
  val orignalList = List(1,2,3)
  val newList= List.foldRight(orignalList, Nil:List[Int])(Cons(_,_))

  // Exercise 3.9
  val len = List.length[Int](List(1,2,3,7,10))
  println("Len is: "+ len)

  val x4= List.foldLeft(List(1,2,3,4),0)(_+_)

  println("X4 is: "+ x4);


  println(List.reverse(List[Int](1,2,3,4,5,6)))


  println("Appended: " + List.appendUsingFold(List[Int](1,2,3,4),5))

}
