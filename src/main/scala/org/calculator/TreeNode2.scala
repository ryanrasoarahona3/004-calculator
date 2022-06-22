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

  /**
   * If expression has brackets
   * @return
   */
  private def hasBrackets(): Boolean = {
    ((expression contains "(") || (expression contains ")"))
  }

  /**
   * Check if an expression is a simple operation
   * @return
   */
  private def isASimpleOperation(): (String, String, String) = {
    val pattern = "^(_|x|([0-9]+(\\.[0-9]+)?))?(\\+|\\*|\\-|\\/)(_|x|([0-9]+(\\.[0-9]+)?))$".r
    if(pattern.matches(expression)){
      var pattern(leftExpr, _l, _l1, operator, rightExpr, _r, _l2) = expression
      leftExpr = if(leftExpr == null) "0" else leftExpr
      (leftExpr, operator, rightExpr)
    }else{
      null
    }
  }

  private def patchAlias(alias:TreeNode2)= {
    left = alias.left
    right = alias.right
    content = alias.content
    funcModificator = alias.funcModificator
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
    }else if(left != null && right != null) {
      if(content == "+"){// addition
        return f(left.evaluate(xVar) + right.evaluate(xVar))
      }else if(content == "*"){
        return f(left.evaluate(xVar) * right.evaluate(xVar))
      }else if(content == "-"){
        return f(left.evaluate(xVar) - right.evaluate(xVar))
      }else if(content == "/"){
        return f(left.evaluate(xVar) / right.evaluate(xVar))
      }
      return 0.0f
    } else{
      return 0.0f
    }
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
      patchAlias(TreeNode2(maskContent))
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
      patchAlias(TreeNode2(masked_expression, d))
    }else{
      val iaso = isASimpleOperation()
      if(iaso != null){
        val (leftExpr, operator, rightExpr) = iaso
        content = operator
        left = TreeNode2(leftExpr, maskContent)
        right = TreeNode2(rightExpr, maskContent)
        print()
      }else{// Not a simple operation but without parenthesis
        val patternAddSub = "(.+)?(\\+|\\-)(.*)".r
        if(patternAddSub.matches(expression)) {
          var patternAddSub(leftExpr, operator, rightExpr) = expression
          leftExpr = if (leftExpr == null) "0" else leftExpr // IN case of negative number
          content = operator
          left = TreeNode2(leftExpr, maskContent)
          right = TreeNode2(rightExpr, maskContent)
          return
        }

        val patternMulDiv = "(.+)?(\\*|\\/)(.*)".r
        if(patternMulDiv.matches(expression)) {
          var patternMulDiv(leftExpr, operator, rightExpr) = expression
          content = operator
          left = TreeNode2(leftExpr, maskContent)
          right = TreeNode2(rightExpr, maskContent)
          return
        }
      }
    }
  }
  init()
}
