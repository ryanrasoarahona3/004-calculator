package org.calculator

import scala.util.matching.Regex

case class TreeNode(expression: String) {
  var left: TreeNode = null
  var right: TreeNode = null
  var content: Any = 0.0f

  /**
   * Permet de vérifier si l'expression est un simple nombre (à ne plus décomposer)
   * Sinon, on le décompose
   *
   * TODO: Trouver un moyen de simplifier le code : comme un héritage de classe ou autres..
   * @return
   */
  private def isASimpleNumber(): Boolean = {
    try {
      expression.toFloat
    } catch {
      case e: NumberFormatException => return false
    }
    true
  }

  private def isNegativeNumber(): Boolean = {
    val pattern = new Regex("\\-[\\d]+")
    pattern matches(expression)
    return false
  }

  /**
   * Cette méthode permet de calculer la valeur saisie
   * @return
   */
  def evaluate(): Float = {
    if(left == null && right == null){
      return content.asInstanceOf[Float]
    }else if(left != null && right != null){
      if(content == "+"){// addition
        return left.evaluate() + right.evaluate()
      }
    }
    throw new Exception("Syntaxe non prise en compte")
  }

  // l'algorithme est ici
  // On utilisera un algorithme récursif
  if(isASimpleNumber()) { // Le cas le plus simple
    left = null
    right = null
    content = expression.toFloat
  }else{
    val pattern = "([0-9]+)(\\+)([0-9]+)".r
    val pattern(leftExpr, operator, rightExpr) = expression
    content = operator
    left = TreeNode(leftExpr)
    right = TreeNode(rightExpr)
  }

  print("Hello world, this is the constructor")
}
