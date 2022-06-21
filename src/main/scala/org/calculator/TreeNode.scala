package org.calculator

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

  private def isASimpleVariable(): Boolean = {
    expression == "x"
  }

  /**
   * Cette méthode permet de calculer la valeur saisie
   * @return
   */
  def evaluate(xVar:Float = 0.0f): Float = {
    if(left == null && right == null){
      if(content == "x"){
        return xVar
      }else {
        return content.asInstanceOf[Float]
      }
    }else if(left != null && right != null){
      if(content == "+"){// addition
        return left.evaluate(xVar) + right.evaluate(xVar)
      }else if(content == "*"){
        return left.evaluate(xVar) * right.evaluate(xVar)
      }else if(content == "-"){
        return left.evaluate(xVar) - right.evaluate(xVar)
      }else if(content == "/"){
        return left.evaluate(xVar) / right.evaluate(xVar)
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
  }else if(isASimpleVariable()){
    left = null
    right = null
    content = "x"
  }else{
    val pattern = "(x|([0-9]+(.[0-9]+)?))(\\+|\\*|\\-|\\/)(x|([0-9]+(.[0-9]+)?))".r
    val pattern(leftExpr, _l, _l1, operator, rightExpr, _r, _l2) = expression
    content = operator
    left = TreeNode(leftExpr)
    right = TreeNode(rightExpr)
  }

  print("Hello world, this is the constructor")
}
