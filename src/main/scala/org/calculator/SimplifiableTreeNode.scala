package org.calculator

import scala.annotation.tailrec

class SimplifiableTreeNode
(var _expression: String, _maskContent: Array[String] = Array(), _needInit: Boolean = true) extends TreeNode2 (_expression, _maskContent, _needInit) {

  /**
   * This function should be called after creating a new object
   */
  override def refreshExpression(): Unit ={
    if(isASimpleNumber()){
      // DON'T TOUCHE
    }else if(isASimpleVariable()){
      // DON'T TOUCH
    }else{
      left.refreshExpression()
      right.refreshExpression()
      expression = left.expression+content+right.expression // Here Content is generally an arithmetic sign
    }
  }

  def swap(): SimplifiableTreeNode ={
    if(content == "+" || content == "-"){
      val o = new SimplifiableTreeNode("", Array(), false)
      o.left = right
      o.right = left
      o.content = content
      o.refreshExpression()
      o
    }else{
      throw new Exception("Only addition and multiplication could be swapped")
    }
  }

}
