

import org.scalatest.funsuite.AnyFunSuite
import org.calculator.TreeNode

class TestTreeNode extends AnyFunSuite {
  // Tests are here
  test("TreeNode.simpleNumber") {
    val p = new TreeNode("3")
    assert(p.evaluate() == 3)
  }
}
