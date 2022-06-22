
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

  test("TreeNode2.simpleFunctionSinAndPlaceholder"){
    val p = TreeNode2("sin_", ""+(math.Pi/2))
    assert(p.evaluate() - 1.0f < 0.00001)
  }

  test("TreeNode2.simpleFunctionCosAndPlaceholder"){
    val p = TreeNode2("cos_", ""+(math.Pi/2))
    assert(p.evaluate() - 0.0f < 0.00001)
  }
}
