
object Chapter4 extends App{

  // exceptions break Referential integrity
  // exceptions causes context dependencies
  // exceptions are not type safe


  //Using sentinel values for error checking -for example
  // returning Double.NAN for 0/0 puts the onus on the
  // client to check the code and is not a good approach


  // Option data type
  //sealed trait Option[+A]
  case class Some[+A] (get:A) extends Option[A]
  case object None extends Option[Nothing]
  // EX 4.1
  trait Option[+A]{
    def map[B] (f:A=>B):Option[B]={
      this match{
        case None => None
        case Some(a) => Some(f(a))
      }
    }
    def flatMap[B](f:A => Option[B]):Option[B]={
       this match{
         case None => None
         case Some(a) => f(a)
       }
    }

    def getOrElse[B >: A](default: =>B):B={
      this match{
        case None => default
        case Some(a) => a
      }
    }

    def orElse[B >: A](obj: => Option[B]):Option[B]={
      this match{
        case None => obj
        case _ => this
      }
    }

    def filter(f:A => Boolean):Option[A]={
      this match{
        case Some(a) if(f(a)) => this
        case _ => None
      }
    }

  }

  // EX 4.2
  def variance(xs:Seq[Double]) :Option[Double]={
    xs.length match{
      case len:Int  if( len > 1) =>  {
        val mean = xs.foldLeft(0.0)(_+_)/len
        Some(xs.map{element => Math.pow((element - mean),2)}.foldLeft(0.0){_+_}/len)
      }
      case _ => None
    }
  }

  def lift[A,B](f:A=>B):Option[A]=>Option[B]={
    _ map f
  }

  // EX 4.3
  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B)=>C):Option[C]={
    (a,b) match{
      case (Some(aVal),Some(bVal)) => Some(f(aVal,bVal))
      case _ => None
    }
  }

  // EX 4.4
  def sequence[A](as:List[Option[A]]):Option[List[A]]={

    as match{
        // this case statement terminates the List
      case Nil => Some(Nil)
        // get the headOption of the list
        // flatMap the head -if it is None, the recursion stops
        // and it returns None, if it is Some(element), call
        // sequence on the rest of the tail and attach the head value
        // to the result using map
      case headOpt::tail =>{
        headOpt flatMap { head:A => sequence(tail) map (head :: _)}

      }
    }

  }

  require(sequence[Int](List(Some(1), Some(2))) == Some(List(1,2)))
  require(sequence[Int](List(Some(1),None ,Some(2))) == None)


  // Exercise 4.6
  sealed trait Either[+E,+A]{
    def map[B](f: A=>B): Either[E,B]={
      this match{
        case Left(e)=> Left(e)
        case Right(a) =>  Right(f(a))
      }
    }
    // Lookup: When mapping over the right side, we must promote the left type
    // parameter to some supertype, to satisfy the +E variance annotation.
    def flatMap[EE >: E, B](f:A=>Either[EE,B]):Either[EE,B]={
      this match {
        case Left(e)=> Left(e)
        case Right(a)=> f(a)
      }

    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]={
       this match {
         case Left(e)=> b
         case Right(a)=> this
       }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] ={
      this match{
        case Left(e)=> Left(e)
        case Right(a) => b match{
          case Left(ee) => Left(ee)
          case Right(b) => Right(f(a,b))
        }
      }
    }


  }
  case class Left[+E](value:E) extends Either[E,Nothing]
  case class Right[+A](value:A) extends Either[Nothing,A]


  def Try[A](a: => A):Either[Exception,A]={
   try Right(a)
    catch {case e:Exception => Left(e)}
  }

  //4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]]={
    def step(as:List[Either[E,A]]):Either[E,List[A]]={
      as match{
        case Left(head:E)::tail =>Left(head)
        case Right(head)::tail => step(tail)
      }
    }

    step(es)
  }




}
