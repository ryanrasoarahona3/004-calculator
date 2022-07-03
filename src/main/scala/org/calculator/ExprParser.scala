package org.calculator

import scala.util.parsing.combinator.JavaTokenParsers

class ExprParser extends JavaTokenParsers {
  def variable: Parser[Any] = "x"
}
