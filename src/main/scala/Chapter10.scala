

object Chapter10 {


  //A Monoid is a purely algebraic data structure, that follows
  // some rules.
  // Monoid is container for the data type -like a List for example
  trait Monoid[A]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1:A, a2:A):A
    // zero or identity function should satisfy op(a,zero) == a
    def zero:A
  }


  // EX 10.1
  class IntAdditionMonoid extends Monoid[Int]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero:Int = 0
  }

  class IntMultiplicationMonoid extends Monoid[Int]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero:Int = 1
  }

  class BooleanOrMonoid extends Monoid[Boolean]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    // zero or identity function should satisfy op(a,zero) == a
    def zero: Boolean = false
  }

  class BooleanAndMonoid extends Monoid[Boolean]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    // zero or identity function should satisfy op(a,zero) == a
    def zero: Boolean = true
  }

  class EndoMonoid[A] extends Monoid[ A => A]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1: (A) => A, a2: (A) => A): (A) => A = {
      a1.compose(a2)
     // or a1 andThen a2
    }
    // zero or identity function should satisfy op(a,zero) == a
    def zero= (a:A) => a
  }

  class OptionMonoid[A] extends Monoid[Option[A]]{
    // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
    def op(a1: Option[A], a2: Option[A]): Option[A] = {
         a1 orElse  a2
    }
    // zero or identity function should satisfy op(a,zero) == a
    def zero: Option[A] = None
  }



  def main(args:Array[String]):Unit={
    val add = new IntAdditionMonoid
    require(add.op(add.op(1,2),3) == add.op(1,add.op(2,3)))
    require(add.op(1, add.zero) == 1)

    val multi = new IntMultiplicationMonoid
    require(multi.op(multi.op(1,2),3) == multi.op(1,multi.op(2,3)))
    require(multi.op(2, multi.zero) == 2)

    val boolOr = new BooleanOrMonoid
    require(boolOr.op(boolOr.op(true,true),false) == boolOr.op(true,boolOr.op(true,false)))
    require(boolOr.op(true, boolOr.zero) == true)
    require(boolOr.op(false, boolOr.zero) == false)

    val boolAnd = new BooleanAndMonoid
    require(boolAnd.op(boolAnd.op(false,true),false) == boolAnd.op(false,boolAnd.op(true,false)))
    require(boolAnd.op(boolAnd.op(true,true),false) == boolAnd.op(true,boolAnd.op(true,false)))
    require(boolAnd.op(true, boolAnd.zero) == true)
    require(boolAnd.op(false, boolAnd.zero) == false)


    val em = new EndoMonoid[Int]

    def f1(a:Int) = {a + 1}
    def f2(a:Int) = {a - 1}
    def f3(a:Int) = {a * 4}

    require(em.op(em.op(f1, f2),f3)(2) == em.op(f1, em.op(f2,f3))(2))


    val optMonoid = new OptionMonoid[Int]
    require(optMonoid.op(optMonoid.op(Some(1), None),Some(2)) == optMonoid.op(Some(1), optMonoid.op(None,Some(2))))

    //EX 10.5
    def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B={
      as.map(f(_)).foldLeft(m.zero)(m.op)
    }


    // this is quite meaningless example -I am not yet entirely
    // sure where foldMap would be useful
    val ss=
    foldMap[Int,Double](List(1,2,3,4), new Monoid[Double] {
      // the operation op should satisfy the associativity i.e. op(op(a,b),c) = op(a,op(b,c))
      def op(a1: Double, a2: Double): Double = a1 + a2

      // zero or identity function should satisfy op(a,zero) == a
      def zero: Double = 0
    })(_.toDouble)

    require(ss==10.0)

   /* EX 10.6 ...
     def foldLeftViaFoldMap[A,B](zero:B)(f: A=>B):B={

    }
    */


    // EX 10.7 -balanced foldMap
    def foldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B={

    }







  }







}
