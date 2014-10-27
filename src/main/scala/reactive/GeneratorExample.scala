package reactive

// Coursera class
object GeneratorExample {

  trait Generator[+T]{
    self =>  // an alias for this
    def generate:T
    def map[S](f:T=>S):Generator[S] = new Generator[S] {

      def generate = f(self.generate)      // you could use Generator.this.generate
    }

    def flatMap[S](f:T=>Generator[S]):Generator[S]= new Generator[S] {
      def generate = f(self.generate).generate
    }

  }

  val integers = new Generator[Int] {
    def generate: Int = scala.util.Random.nextInt()
  }

  val booleans = new Generator[Boolean] {
    def generate: Boolean = integers.generate > 0
  }

  val pairs = new Generator[(Int,Int)] {
    def generate: (Int, Int) = (integers.generate, integers.generate)
  }


  def pairsImproved[T,U](t:Generator[T], u:Generator[U])= for{
    x <- t
    y <- u
  } yield(x,y)


  def main(args:Array[String])={
    println(pairsImproved[Boolean,Int](booleans,integers))
    println(lists)
  }

  def Single[T](x:T) = new Generator[T] {
    // an alias for this
    def generate: T = x
  }

  def choose (lo:Int, hi:Int):Generator[Int]={
   for (x <- integers)
      yield lo + x % (hi-lo)
   }

  def oneOf[T](xs:T*): Generator[T]={
    for (idx <- choose(0, xs.length))
      yield xs(idx)
  }


  def lists:Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if(isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = Single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail



}
