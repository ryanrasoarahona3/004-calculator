
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

  test("TreeNode2.testBracketWithinCos"){
    val p = TreeNode2("cos(0)")
    assert(p.evaluate() - 1.0f < 0.00001)
  }

  test("TreeNode2.testBracketWithinCosAndVariable"){
    val p = TreeNode2("cos(x)")
    assert(p.evaluate(0.0f) - 1.0f < 0.00001)
  }

  test("TreeNode2.nombreNegatif") {
    val p = new TreeNode2("-90")
    assert(p.evaluate() == -90.0f) // .0f Float / .d Double
  }

  test("TreeNode.3p3") {
    val p = new TreeNode2("3+3")
    assert(p.evaluate() == 6.0f) //
  }

  test("TreeNode.3*3") {
    val p = new TreeNode2("3*3")
    assert(p.evaluate() == 9.0f)
  }

  test("TreeNode.3-3") {
    val p = new TreeNode2("4-3")
    assert(p.evaluate() == 1.0f)
  }

  test("TreeNode.6/3"){
    val p = TreeNode2("6/3")
    assert(p.evaluate() == 2.0f)
  }

  test("TreeNode.3.0+5"){
    val p = TreeNode2("3.0+5")
    assert(p.evaluate() == 8.0f)
  }

  test("TreeNode.3.3*3.3"){
    val p = TreeNode2("3.3*3.3")
    assert(p.evaluate() == 3.3f*3.3f)
  }

}
