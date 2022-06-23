
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
    val p = new TreeNode2("_", Array("5.0"));
    assert(p.evaluate() == 5.0f)
  }

  test("TreeNode2.simpleFunctionSinAndPlaceholder"){
    val p = TreeNode2("sin_", Array(""+(math.Pi/2)))
    assert(p.evaluate() - 1.0f < 0.00001)
  }

  test("TreeNode2.simpleFunctionCosAndPlaceholder"){
    val p = TreeNode2("cos_", Array(""+(math.Pi/2)))
    assert(p.evaluate() - 0.0f < 0.00001)
  }

  // HERE
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

  test("TreeNode.3+x"){
    val p = TreeNode2("3+x")
    assert(p.evaluate(10.0f) == 13.0f)
  }

  test("TreeNode.3*x"){
    val p = TreeNode2("3*x")
    assert(p.evaluate(10.0f) == 30.0f)
  }

  test("TreeNode.drawing"){
    val p = new TreeNode2("x")
    val o = p.evaluateWithinInterval(0.0f, 1.0f, 0.5f)
    assert(o(1)._2 == 0.5f)
    assert(o(2)._2 == 1.0f)
  }

  test("TreeNode.priorityOfMultiplicationOverAddition"){
    val p = new TreeNode2("2*3+1")
    assert(p.evaluate() == 7.0f)
  }

  test("TreeNode.priorityOfMultiplicationOverAddition2"){
    val p = new TreeNode2("1+2*3")
    assert(p.evaluate() == 7.0f)
  }

  test("TreeNode.sin2x"){
    val p = new TreeNode2("sin(2*x)")
    assert(p.evaluate(math.Pi.toFloat/4) - 1.0 < 0.0001)
  }

  test("TreeNode.sin(x-1)"){
    val p = new TreeNode2("sin(x-1)")
    assert(p.evaluate((math.Pi.toFloat/2)+1) - 1.0 < 0.0001)
  }

  test("TreeNode.sin2x+1"){
    val p = new TreeNode2("sin(2*x+1)")
    assert(p.evaluate((math.Pi.toFloat/4)-0.5f) - 1.0 < 0.0001)
  }

  test("TreeNode2.fonctionAvecParentheses"){
    val p = TreeNode2("(2+2)*3")
    assert(p.evaluate() == 12.0f)
  }

  test("TreeNode2.fonctionAvecParentheses2"){
    val p = TreeNode2("(2+2)*3+1")
    assert(p.evaluate() == 13.0f)
  }

  test("TreeNode2.fonctionAvecParenthesesEtX"){
    val p = TreeNode2("(2+2)*3+x")
    assert(p.evaluate(1.0f) == 13.0f)
  }

  test("TreeNode2.sin(4xx)"){
    val p = new TreeNode2("sin(4*x*x)")
    assert(p.evaluate(2.0f) - math.sin(4*2*2) < 0.001)
  }

  test("TreeNode2.4xx"){
    val p = new TreeNode2("4*x*x")
    assert(p.evaluate(2.0f) == 4*2*2.0f)
  }

  test("TreeNode2.sincosadd"){
    val p = TreeNode2("sin(x)+cos(x)")
    assert(p.evaluate(2.0f) - math.sin(2.0)+math.cos(2.0) < 0.0001)
  }

  test("TreeNode2.complicatedFunction"){
    val p = TreeNode2("sin(x)+cos(10*x)")
    assert(p.evaluate(0.5f) - (math.sin(0.5) + math.cos(0.5*10)).toFloat < 0.0001)
  }

  test("TreeNode2.multiplePlaceholder"){
    val p = TreeNode2("_+_", Array("2.0", "1.0"))
    assert(p.evaluate() == 3.0f)
  }

  test("TreeNode2.multiplePlaceholderWithFunction"){
    val p = TreeNode2("sin_+cos_", Array("x", "10*x"))
    assert(p.evaluate(0.5f) - (math.sin(0.5) + math.cos(0.5*10)).toFloat < 0.0001)
  }

  // 1.5*cos(5*(x*x))
  // sin(x) + cos(x)
  // x*x*x
  // 3*3*3
}
