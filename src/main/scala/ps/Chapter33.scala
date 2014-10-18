package ps


import scala.util.parsing.combinator._

class Arith extends JavaTokenParsers{
  def expr: Parser[Any] = term ~ rep( "+" ~ term | "-" ~ term)
  def term: Parser[Any] = factor ~ rep("*" ~ term | "/" ~ term)
  def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
}


class JSON extends JavaTokenParsers{
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"


  def obj: Parser[Any] =
    "{"~repsep(member,",")~"}"
  //^^ { case "{"~ms~"}" => Map() ++ ms }

  def arr: Parser[Any] = "[" ~ repsep(value,",") ~ "]"
  def member: Parser[Any] = stringLiteral ~":"~value


}

object Chapter33 {



    def main(args: Array[String]) {
      println("input : "+ args(0))
      val arith = new Arith
      println(arith.parseAll(arith.expr, args(0)))

      val json = new JSON
      println(json.parseAll(json.value,"{ \"a\" : 1 , \"b\" : 2}"))

      val pt= json.parseAll(json.value,"{ \"a\" : 1 , \"b\" : 2}")




    }

}
