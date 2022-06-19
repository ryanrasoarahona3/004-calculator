package org.calculator

case class TreeNode(expression: String) {
  var left: TreeNode = null
  var right: TreeNode = null
  var content: Float = 0.0f

  /**
   * Permet de vérifier si l'expression est un simple nombre (à ne plus décomposer)
   * Sinon, on le décompose
   *
   * TODO: Trouver un moyen de simplifier le code : comme un héritage de classe ou autres..
   * @return
   */
  private def isASimpleNumber(): Boolean = {
    try {
      expression.toFloat()
    } catch {
      case e: NumberFormatException => return false
    }
    true
  }

  /**
   * Cette méthode permet de calculer la valeur saisie
   * @return
   */
  def evaluate(): Float = {
    if(left == null && right == null){
      return content
    }
    throw new Exception("Syntaxe non prise en compte")
  }

  // l'algorithme est ici
  // On utilisera un algorithme récursif
  if(isASimpleNumber()) { // Le cas le plus simple
    left = null
    right = null
    content = expression.toFloat
  }

  print("Hello world, this is the constructor")
}
