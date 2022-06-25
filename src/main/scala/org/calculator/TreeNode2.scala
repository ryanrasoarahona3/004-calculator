package org.calculator

case class TreeNode2(var expression: String, maskContent: Array[String] = Array(), needInit: Boolean = true) {

  var left: TreeNode2 = null
  var right: TreeNode2 = null
  var content: Any = 0.0f

  var funcModificator = "identity"

  /**
   * This function is only used within SimplifiableExpression
   * A Trait can be also used
   */
  def refreshExpression(): Unit = null

  /**
   * Permet de vérifier si l'expression est un simple nombre (à ne plus décomposer)
   * Sinon, on le décompose
   * @return
   */
  def isASimpleNumber(): Boolean = {
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
  def isASimpleVariable(): Boolean = {
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
    val pattern = "^(sin_|cos_|_|x|([0-9]+(\\.[0-9]+)?))?(\\+|\\*|\\-|\\/)(sin_|cos_|_|x|([0-9]+(\\.[0-9]+)?))$".r
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
      // Very important
      patchAlias(TreeNode2(maskContent(0))) // expect that maskContent length equals 1
      if (isASimpleFunction()) {
        val pattern = "(sin|cos)_".r
        val pattern(funcName) = expression
        funcModificator = funcName
      }
    }else if(hasBrackets()){
      var bracket_level = 0
      var sub_added = false
      var in_sub = false
      var sub_expressions:Array[String] = Array()
      var _sub_expr = ""

      var masked_expression:String = ""
      //var sub_expression: String = "" // Between brackets
      for(c <- expression){
        if(c == '(') bracket_level+=1
        if(bracket_level == 0/* || sub_added*/){
          masked_expression+=c
          if(in_sub){
            sub_expressions = sub_expressions :+ _sub_expr
            _sub_expr = ""
          }
          in_sub = false
        }
        if(bracket_level > 0){
          if(!in_sub)
            masked_expression+= "_"
          in_sub = true
          /*if(sub_expression == "")
            masked_expression+= "_"*/
          _sub_expr+= c
        }
        if(c == ')') bracket_level-=1
        if(c == ')' && bracket_level == 0) sub_added = true
      }
      if(sub_expressions.length > 0) {
        if(sub_expressions.last != _sub_expr && _sub_expr != "")
          sub_expressions = sub_expressions :+ _sub_expr
      } else if (_sub_expr != "") {
        sub_expressions = sub_expressions :+ _sub_expr
      }

      var ds: Array[String] = Array()
      for(sub_expr <- sub_expressions){
        val pattern = "\\((.*)\\)".r
        var pattern(d) = sub_expr
        ds = ds:+ d
      }
      patchAlias(TreeNode2(masked_expression, ds))
      // Use For loop
      //var pattern(d) = sub_expression
      //patchAlias(TreeNode2(masked_expression, d))
    }else{
      val iaso = isASimpleOperation()
      if(iaso != null){
        val (leftExpr, operator, rightExpr) = iaso
        content = operator
        left = TreeNode2(leftExpr, maskContent.slice(0, leftExpr.count(_ == '_')))
        right = TreeNode2(rightExpr, maskContent.takeRight(rightExpr.count(_ == '_')))
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
  if(needInit)
    init()

  private def isConstant(): Boolean ={
    if(expression contains("x"))
      return false
    return true
  }

  def simplify(): TreeNode2 = {
    if (left == null && right == null) {
      return this.copy()
    } else if(isConstant()) {
      return TreeNode2(""+evaluate())
    } else { // Variable
      if(true){
        val o = TreeNode2("", Array(), false)
        o.left = TreeNode2("", Array(), false)
        o.left.left = left.derivate()
        o.left.content = content
        o.left.right = right.derivate()
        o.left = o.left.simplify()
        return o
      }
        /*if(content == "+"){
          // même dérivé -> additionnable
          // Dérivé additionnable -> additionnable
          var sumOfDerivatives = TreeNode2("", Array(), false)
          sumOfDerivatives.left = left.derivate().simplify()
          sumOfDerivatives.right = right.derivate().simplify()
          sumOfDerivatives.content = content
          sumOfDerivatives = sumOfDerivatives.simplify()

          if(sumOfDerivatives.isASimpleNumber()){
            val o = TreeNode2("", Array(), false)

            val variablePart = TreeNode2("", Array(), false)
            variablePart.content = "/"

            return o
          }else{

          }

        }
      }*/
      /*if(left.derivate().expression == right.derivate().expression) {
        // Une expression qui peut être utile
        val o = TreeNode2("", Array(), false)
        o.left = TreeNode2("", Array(), false)
        o.left.left = left.derivate()
        o.left.content = content
        o.left.right = right.derivate()
        o.left = o.left.simplify()
        print()

      }*/else{
        val o = this.copy()
        o.left = o.left.simplify()
        o.right = o.right.simplify()
        o.expression = o.left.expression + o.content + o.right.expression
        return o
      }
    }
    return null
  }

  def derivate(): TreeNode2 = {
    if(funcModificator == "identity") {
      if (left == null && right == null) {
        if (content == "x") {
          return TreeNode2("1.0")
        } else {
          return TreeNode2("0.0")
        }
      } else if (left != null && right != null) {
        val o = TreeNode2("", Array(), false)
        if (content == "+" || content == "-") {
          o.left = left.derivate()
          o.right = right.derivate()
          o.content = content
          o.expression = o.left.expression + o.content + o.right.expression
        } else if (content == "*") {
          val oleft = TreeNode2("", Array(), false)
          val oright = TreeNode2("", Array(), false)

          oleft.left = left
          oleft.content = "*"
          oleft.right = right.derivate()

          oright.left = left.derivate()
          oright.content = "*"
          oright.right = right

          o.content = "+"
          o.left = oleft
          o.right = oright
        } else if (content == "/") {
          val ou = TreeNode2("", Array(), false)
          val ouleft = TreeNode2("", Array(), false)
          val ouright = TreeNode2("", Array(), false)
          val od = TreeNode2("", Array(), false)

          ouleft.left = left.derivate()
          ouleft.content = "*"
          ouleft.right = right
          ou.content = "-"
          ouright.left = left
          ouright.content = "*"
          ouright.right = right.derivate()
          ou.left = ouleft
          ou.right = ouright

          o.content = "/"
          od.left = right
          od.content = "*"
          od.right = right

          o.left = ou
          o.right = od
        }
        return o
      }
    }else{
      val o = TreeNode2("", Array(), false)
      if(funcModificator == "sin"){
        val this_identity = this.copy()
        this_identity.funcModificator = "identity"
        this_identity.expression = ""
        val oleft = this_identity.derivate()
        o.content = "*"
        val oright = this.copy()
        oright.funcModificator = "cos"
        oright.expression = ""
        o.left = oleft
        o.right = oright
      }else if(funcModificator == "cos"){
        val so = TreeNode2("", Array(), false)
        val this_identity = this.copy()
        this_identity.funcModificator = "identity"
        this_identity.expression = ""
        val oleft = this_identity.derivate()
        so.content = "*"
        val oright = this.copy()
        oright.funcModificator = "sin"
        oright.expression = ""
        so.left = oleft
        so.right = oright

        o.left = TreeNode2("-1")
        o.content = "*"
        o.right = so
      }
      return o
    }
    return null
  }

  def getExpression(): String ={
    if(left == null && right == null && funcModificator == "identity"){
      // Constant number or x
      return ""+content
    }else if(left != null && right != null && funcModificator == "identity"){
      if(content == "*"){
        // Addition within multiplication (need brackets)
        var leftExpr = left.getExpression()
        var rightExpr = right.getExpression()
        if((left.content == "+" || left.content == "-") && left.funcModificator == "identity"){
          leftExpr = "(" + leftExpr + ")"
        }
        if((right.content == "+" || right.content == "-") && right.funcModificator == "identity"){
          rightExpr = "(" + rightExpr + ")"
        }
        return leftExpr+content+rightExpr
      }else{
        // Only addition
        return left.getExpression()+content+right.getExpression()
      }
    }else if(funcModificator != "identity"){
      val tn = TreeNode2("", Array(), false)
      tn.content = content
      tn.left = left
      tn.right = right
      tn.funcModificator = "identity"
      return funcModificator+"("+tn.getExpression()+")"
    }
    return ""
  }

  private def isScalar(): Boolean ={
    try {
      getExpression().toFloat
    } catch {
      case e: NumberFormatException => return false
    }
    true
  }

  private def isX(): Boolean ={
    return getExpression() == "x"
  }

  private def isAXForm(): Boolean ={
    if(content == "*"){
      if(left.isScalar() && right.isX()) return true
      if(right.isScalar() && left.isX()) return true
    }else if(content == "/"){
      if(left.isX() && right.isScalar()) return true
    }
    false
  }

  def getSimplified(): TreeNode2 ={
    if(left == null && right == null){
      return this
    }else if(!(getExpression() contains "x")){
      // Constant
      return TreeNode2(""+evaluate())
    }else if(getExpression() contains "x"){
      // Variable
      val o = TreeNode2("", Array(), false)
      o.left = left.getSimplified()
      o.content = content
      o.right = right.getSimplified()
      o.funcModificator = funcModificator

      // Sorting
      if(o.left.isScalar() && o.right.isX()){
        // Swap
        val __ = o.left
        o.left = o.right
        o.right = __
      }

      if(left.content == "+" && content == "+" && left.right.isScalar() && right.isScalar()){
        val _l = left.left
        val _r = TreeNode2("", Array(), false)
        _r.content = "+"
        _r.left = left.right
        _r.right = right
        left = _l
        right = _r
        return getSimplified()
      }

      // Swap multiplication
      if(content == "*" && left.isX() && right.isScalar()){
        // Swap
        val __ = o.left
        o.left = o.right
        o.right = __
      }

      // Multiplication By Zero
      if(left.getExpression() == "0.0" && right.isX()){
        return TreeNode2("0.0")
      }

      return o
    }
    null
  }
}
