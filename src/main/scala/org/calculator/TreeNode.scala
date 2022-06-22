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

  private def isASimpleOperation(): (String, String, String) = {
    val pattern = "(x|([0-9]+(.[0-9]+)?))(\\+|\\*|\\-|\\/)(x|([0-9]+(.[0-9]+)?))".r
    if(pattern.matches(expression)){
      val pattern(leftExpr, _l, _l1, operator, rightExpr, _r, _l2) = expression
      (leftExpr, operator, rightExpr)
    }else{
      null
    }
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

  def evaluateWithinInterval(min:Float, max:Float, interval:Float): Array[(Float, Float)] ={
    var output: Array[(Float,Float)] = Array()
    var _x = min
    while(_x <= max){
      val _y = evaluate(_x)
      output = output :+ (_x, _y)
      _x+= interval
    }
    return output
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
    val iaso = isASimpleOperation()
    if(iaso != null){
      val (leftExpr, operator, rightExpr) = iaso
      content = operator
      left = TreeNode(leftExpr)
      right = TreeNode(rightExpr)
      print()
    }else{
      // TODO: Should do without parenthesis
      // Uniquement repérer les additions et les soustractions
      val pattern = "(.+)(\\+|\\-)(.*)".r
      val pattern(leftExpr, operator, rightExpr) = expression
      content = operator
      left = TreeNode(leftExpr)
      right = TreeNode(rightExpr)
    }
  }

  /*else{
    val pattern = "(x|([0-9]+(.[0-9]+)?))(\\+|\\*|\\-|\\/)(x|([0-9]+(.[0-9]+)?))".r
    val pattern(leftExpr, _l, _l1, operator, rightExpr, _r, _l2) = expression
    content = operator
    left = TreeNode(leftExpr)
    right = TreeNode(rightExpr)
  }*/

  print("Hello world, this is the constructor")
}
