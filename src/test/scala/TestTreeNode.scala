

import org.scalatest.funsuite.AnyFunSuite
import org.calculator.TreeNode

class TestTreeNode extends AnyFunSuite {
  // Tests are here
  test("TreeNode.simpleNumber") {
    val p = new TreeNode("3")
    assert(p.evaluate() == 3)
  }

  test("TreeNode.nombreNegatif") {
    val p = new TreeNode("-90")
    assert(p.evaluate() == -90.0f) // .0f Float / .d Double
  }

  test("TreeNode.3p3") {
    val p = new TreeNode("3+3")
    assert(p.evaluate() == 6.0f) //
  }
}
