package org.calculator

case class TreeNode2(expression: String, maskContent: String = "") {

  var left: TreeNode2 = null
  var right: TreeNode2 = null
  var content: Any = 0.0f

  var funcModificator = "identity"

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

  /**
   * Note that placeholder is required for this kind of expression
   * @return
   */
  private def isASimpleFunction(): Boolean = {
    val pattern = "(sin|cos)_".r
    pattern.matches(expression)
  }

  private def hasBrackets(): Boolean = {
    ((expression contains "(") || (expression contains ")"))
  }

  def evaluate(xVar:Float = 0.0f): Float = {
    var f:Float=>Float = null
    if(funcModificator == "identity") f = (x:Float)=>x
    if(funcModificator == "sin") f = (x:Float)=>math.sin(x).toFloat
    if(funcModificator == "cos") f = (x:Float)=>math.cos(x).toFloat
    if(left == null && right == null){
      if(content == "x"){
        f(xVar)
      } else {
        f(content.asInstanceOf[Float])
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
    }else if(isASimplePlaceholder() || isASimpleFunction()) {
      // Create an alias
      val alias = TreeNode2(maskContent)
      left = alias.left
      right = alias.right
      content = alias.content
      if (isASimpleFunction()) {
        val pattern = "(sin|cos)_".r
        val pattern(funcName) = expression
        funcModificator = funcName
        print()
      }
    }else if(hasBrackets()){
      var bracket_level = 0
      var sub_added = false
      var masked_expression:String = ""
      var sub_expression: String = "" // Between brackets
      for(c <- expression){
        if(c == '(') bracket_level+=1
        if(bracket_level == 0 || sub_added){
          masked_expression+=c
        }else{
          if(sub_expression == "")
            masked_expression+= "_"
          sub_expression+= c
        }
        if(c == ')') bracket_level-=1
        if(c == ')' && bracket_level == 0) sub_added = true
      }
      val pattern = "\\((.*)\\)".r
      var pattern(d) = sub_expression


      // Patching parameters with alias
      val alias = TreeNode2(masked_expression, d)
      left = alias.left
      right = alias.right
      content = alias.content
      funcModificator = alias.funcModificator
      if (isASimpleFunction()) {
        val pattern = "(sin|cos)_".r
        val pattern(funcName) = expression
        funcModificator = funcName
      }


    }
  }
  init()
}
