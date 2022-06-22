package org.calculator

case class TreeNode2(expression: String, maskContent: String = "") {

  var left: TreeNode2 = null
  var right: TreeNode2 = null
  var content: Any = 0.0f

  /**
   * Permet de vérifier si l'expression est un simple nombre (à ne plus décomposer)
   * Sinon, on le décompose
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

  /**
   * Est la variable X
   * @return
   */
  private def isASimpleVariable(): Boolean = {
    expression == "x"
  }

  /**
   * Est une masque à remplacer par maskContent
   * @return
   */
  private def isASimplePlaceholder(): Boolean = {
    val pattern = "_+".r
    pattern.matches(expression)
  }

  def evaluate(xVar:Float = 0.0f): Float = {
    if(left == null && right == null){
      if(content == "x"){
        xVar
      } else {
        content.asInstanceOf[Float]
      }
    }else{
      return 0.0f
    }
  }

  def init(): Unit ={
    if(isASimpleNumber()) { // Le cas le plus simple
      left = null
      right = null
      content = expression.toFloat
    }else if(isASimpleVariable()){
      left = null
      right = null
      content = "x"
    }else if(isASimplePlaceholder()){
      // Create an alias
      var alias = TreeNode2(maskContent)
      left = alias.left
      right = alias.right
      content = alias.content
    }
  }
  init()
}
