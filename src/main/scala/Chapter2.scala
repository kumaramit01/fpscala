import scala.annotation.tailrec

object Chapter2 extends App{


  def fact(n:Int):Int={
    @tailrec
    def factAccum(num:Int, accum:Int):Int={
      if(num>n)
        accum
      else{
        factAccum(num+1,accum*num)
      }
    }
    factAccum(1,1)
  }


  println("fact(3)=" +fact(3))

  /**
   * we maintain l1 and l2 as the fib values for
   * the n-2 and n-1
   * @param n
   * @return
   */
  def fib(n:Int):Int={
    @tailrec
    def fibAccum(num:Int, l1:Int, l2:Int):Int={
      if(num ==n){
        l1 + l2
      }else if(num < n && num > 1){
        fibAccum(num+1, l2, l2+l1)
      }else{
        // for 0 and 1 fib is same as the num
        fibAccum(num+1, 0, num)
      }


    }
    fibAccum(0,0,0);
  }
  println("fib(8)=" +fib(8))



  def isSorted[A](as:Array[A], f:(A,A)=>Boolean):Boolean={
   @tailrec
    def sorted(index:Int):Boolean={
      if(index>=(as.size-1)) true
      else{
       if(f(as(index),as(index+1))){
         sorted(index+1)
       }else{
         false
       }
      }
    }
    sorted(0)
  }
  println(isSorted[Int](Array[Int](1,2,3,4,5,6,7,8),(a:Int,b:Int) => a < b))


  /*Higher order functions*/

  /*function that takes[ a ], and a [function that takes a and b and returns C] and returns a
   function that takes b and returns C*/
  def partial1[A,B,C](a:A,f:(A,B)=>C):(B=>C)={
    b:B => f(a,b)
  }

  def curry[A,B,C](f:(A,B)=>C): A=> (B=>C)={
    // a:A => f(a,_)
    a:A => b:B => f(a,b)
  }

  println(curry((a:Int,b:Int)=>a+b))

  def uncurry[A,B,C](f:A=>B=>C):(A,B)=>C={
    f(_)(_)
  }



  def compose[A,B,C](f:B=>C, g:A=>B):A=>C={
   a:A=> f(g(a))
  }





}
