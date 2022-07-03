import org.calculator.ExprParser
import org.scalatest.funsuite.AnyFunSuite

import scala.util.parsing.combinator._

/*
class Arith extends JavaTokenParsers {
  //def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  //def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  //def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
  def variable: Parser[Any] = "x"
}
 */

class ParseExpr extends ExprParser {
  val a = "x"
  def main(args: Array[String]) {
    println("input : "+ a)
    val o = parseAll(variable, a)
    println(parseAll(variable, a))
  }

  def parseIt(a: String): ParseResult[Any] ={
    return parseAll(variable, a)
  }
}

class TestSPC extends AnyFunSuite {

  test("spc.basique"){
    val a = "x"
    val parser = new ParseExpr()
    val o = parser.parseIt(a)
    assert("x" == o.get)
  }
}
