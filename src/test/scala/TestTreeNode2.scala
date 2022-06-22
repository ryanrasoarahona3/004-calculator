
import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestTreeNode2 extends AnyFunSuite {

  test("TreeNode2.simpleNumber") {
    val p = new TreeNode2("3")
    assert(p.evaluate() == 3)
  }

  test("TreeNode2.simpleVariable") {
    val p = new TreeNode2("x")
    assert(p.evaluate(2.0f) == 2.0f)
  }

  test("TreeNode2.simplePlaceholder") {
    val p = new TreeNode2("_", "5.0");
    assert(p.evaluate() == 5.0f)

  }
}
