
import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestTreeNode2 extends AnyFunSuite {
  test("TreeNode.simpleNumber") {
    val p = new TreeNode2("3")
    assert(p.evaluate() == 3)
  }
}
