package reactive

/**
 * Data structures with map and flatmap togather with some algebraic laws
 */
object Monads {

  // Associativity( m flatmap) f flatmap g == m
  // Left unit unit(x) flatMap f = f(x)
  // Right unit m flatMap unit =   m

  trait M[T]{
    // called bind
    def flatMap[U](f:T =>M[U]):M[U]
    def unit[T](x:T):M[T]
    def map[U](f:T=>U):M[U]= flatMap(f andThen unit)
  }

}
