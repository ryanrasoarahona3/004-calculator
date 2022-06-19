package org.calculator


/**
 * À ne plus utiliser
 * @deprecated Cette classe devrait être supprimée dans un futur proche
 */
class Parser(val expression: String) {

  def isNumber(): Boolean = {
    try {
      expression.toFloat
    } catch {
      case e: NumberFormatException => return false
    }
    true
  }

  override def toString: String = super.toString + " : " + expression
}

