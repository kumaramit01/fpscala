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
    val rand = new java.util.Random
    def generate: Int = rand.nextInt()
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




}
