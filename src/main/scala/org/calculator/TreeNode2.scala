package org.calculator

import scala.util.parsing.combinator.JavaTokenParsers

case class TreeNode2(var expression: String, maskContent: Array[String] = Array(), needInit: Boolean = true)
extends JavaTokenParsers {

  /**
   * Here are Scala combination parser functions
   */
  def p_variable: Parser[Any] = "x"
  def p_placeholder: Parser[Any] = "_".r
  def p_float: Parser[Any] = "\\d+(\\.\\d+)?".r
  def p_sincos: Parser[Any] = "(sin|cos)_".r

  def p_validOperandL: Parser[Any] = "(sin_|cos_|_|x|([0-9]+(\\.[0-9]+)?))?".r
  def p_validOperator: Parser[Any] = "(\\+|\\*|\\-|\\/)".r
  def p_validOperandR: Parser[Any] = "(sin_|cos_|_|x|([0-9]+(\\\\.[0-9]+)?))".r
  def p_simpleOperation: Parser[Any] = p_validOperandL~p_validOperator~p_validOperandR

  def p_haveBracketsL: Parser[Any] = ".*\\(.*".r
  def p_haveBracketsR: Parser[Any] = ".*\\).*".r
  def p_haveBrackets: Parser[Any] = p_haveBracketsL | p_haveBracketsR

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
      parseAll(p_float, expression).get.toString.toFloat
    } catch {
      case e: NumberFormatException => return false
      case e: RuntimeException => return false
    }
    true
  }

  /**
   * Est la variable X
   * @return
   */
  def isASimpleVariable(): Boolean = {
    try {
      return "x" == parseAll(p_variable, expression).get
    } catch {
      case e: RuntimeException => return false
    }
  }

  /**
   * Est une masque à remplacer par maskContent
   * @return
   */
  private def isASimplePlaceholder(): Boolean = {
    // val pattern = "_+".r
    //pattern.matches(expression)
    try {
      return "_" == (parseAll(p_placeholder, expression).get)
    } catch {
      case e: RuntimeException => return false
    }
  }

  /**
   * Note that placeholder is required for this kind of expression
   * @return
   */
  private def isASimpleFunction(): Boolean = {
    try {
      parseAll(p_sincos, expression).get
      return true
    } catch {
      case e: RuntimeException => return false
    }
  }

  /**
   * If expression has brackets
   * @return
   */
  private def hasBrackets(): Boolean = {
    try{
      parseAll(p_haveBrackets, expression).get
      return true
    }catch{
      case e: RuntimeException => return false
    }
    return false
    //((expression contains "(") || (expression contains ")"))
  }

  /**
   * Check if an expression is a simple operation
   * @return
   */
  private def isASimpleOperation(): (String, String, String) = {
    val pattern = "^(sin_|cos_|_|x|([0-9]+(\\.[0-9]+)?))?(\\+|\\*|\\-|\\/)(sin_|cos_|_|x|([0-9]+(\\.[0-9]+)?))$".r
    var o: ParseResult[Any] = null
    try{
      o = parseAll(p_simpleOperation, expression)
    }catch{
      case e: RuntimeException => return null
    }

    if(o.successful){
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
      }else if(content == "-" && left.getExpression() == "0.0"){
        if(right.getExpression() == "*" || right.getExpression() == "/")
          return "-("+right.getExpression()+")"
        return "-"+right.getExpression()
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

  private def isAXForm(): Boolean = {
    if(getExpression() == "x") return true
    if(content == "*"){
      if(left.isScalar() && right.isX()) return true
      if(right.isScalar() && left.isX()) return true
    }else if(content == "/"){
      if(left.isX() && right.isScalar()) return true
    }
  false
  }

  private def getAXForm(): TreeNode2 ={
    if(getExpression() == "x") return TreeNode2("1.0")
    if(content == "*"){
      if(left.isScalar() && right.isX()) return left
      if(right.isScalar() && left.isX()) return right
    }else if(content == "/"){
      if(left.isX() && right.isScalar()){
        val o = TreeNode2("", Array(), false)
        o.left = TreeNode2("1.0")
        o.content = "/"
        o.right = right
        return o
      }
    }
    null
  }

  def getSimplified(): TreeNode2 ={
    if(left == null && right == null){
      return this
    }else if(!(getExpression() contains "x")){
      // Constant
      return TreeNode2(""+evaluate())
    }else if(funcModificator == "identity"){
      if(getExpression() contains "x"){
        // Variable
        val o = TreeNode2("", Array(), false)
        o.left = left.getSimplifiedLoop()
        o.content = content
        o.right = right.getSimplifiedLoop()
        o.funcModificator = funcModificator

        left = o.left
        content = o.content
        right = o.right

        if(content == "+" || content == "-"){

          // Sorting
          if(o.left.isScalar() && o.right.isX()){
            // Swap
            val __ = o.left
            o.left = o.right
            o.right = __
            return o.getSimplifiedLoop()
          }

          // Addition with zero
          if(right.getExpression() == "0.0"){
            return left
          }

          // Ax+bx
          if(left.isAXForm() && right.isAXForm()){
            val o = TreeNode2("", Array(), false)
            val _ol = TreeNode2("", Array(), false)
            _ol.content = content // + or -
            _ol.left = left.getAXForm()
            _ol.right = right.getAXForm()
            o.left = _ol
            o.content = "*"
            o.right = TreeNode2("x")
            return o
          }
        }

        // Nested addition
        if(left.content == "+" && left.funcModificator == "identity" && content == "+" && left.right.isScalar() && right.isScalar()){
          val _l = left.left
          val _r = TreeNode2("", Array(), false)
          _r.content = "+"
          _r.left = left.right
          _r.right = right
          left = _l
          right = _r
          return getSimplifiedLoop()
        }
        // Nested multiplication 1
        if(right.content == "*" && right.funcModificator == "identity" && content == "*" && left.isScalar() && right.left.isScalar()){
          val _l = TreeNode2("", Array(), false)
          val _r = right.right
          _l.content = "*"
          _l.left = left
          _l.right = right.left
          left = _l
          right = _r
          return getSimplifiedLoop()
        }
        // Nested multiplication 2
        if(content == "*" && left.content == "*" && right.funcModificator == "identity"){
          if(left.left.isScalar() && right.isScalar()){
            val _l = TreeNode2("", Array(), false)
            val _r = left.right
            _l.content = "*"
            _l.left = left.left
            _l.right = right
            left = _l
            right = _r
            return getSimplifiedLoop()
          }
        }

        // Nested substraction (special case)
        if(content == "+" && funcModificator == "identity"){
          if(right.content == "-"){
            if(right.left.isScalar() && right.left.content == 0.0f) {
              content = "-"
              right = right.right
              return getSimplifiedLoop()
            }
          }
          if(right.content == "*"){
            if(right.left.isScalar() && right.left.content == -1.0f) {
              content = "-"
              right = right.right
              return getSimplifiedLoop()
            }
          }
        }

        // Swap multiplication
        if(content == "*" && left.isX() && right.isScalar()){
          // Swap
          val __ = o.left
          o.left = o.right
          o.right = __
        }
        if(content == "*") {
          // Multiplication By Zero
          if (left.getExpression() == "0.0" && right.isX()) {
            return TreeNode2("0.0")
          }
          // MUltiplication By One
          if (left.getExpression() == "1.0") {
            return right
          }
        }



        return o
      }
    }else{
      val o = TreeNode2("", Array(), false)
      o.left = left
      o.content = content
      o.right = right
      o.funcModificator = "identity" // Important
      val p = o.getSimplifiedLoop()
      p.funcModificator = funcModificator
      return p
    }
    null
  }

  def getSimplifiedLoop(): TreeNode2 ={
    var prevExpr = "NULL"
    var t = this
    var expr = t.getExpression()
    while(prevExpr != expr){
      prevExpr = expr
      t = t.getSimplified()
      expr = t.getExpression()
    }
    return t
  }
}
