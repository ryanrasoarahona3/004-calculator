

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

  test("TreeNode.3*3") {
    val p = new TreeNode("3*3")
    assert(p.evaluate() == 9.0f)
  }

  test("TreeNode.3-3") {
    val p = new TreeNode("4-3")
    assert(p.evaluate() == 1.0f)
  }

  test("TreeNode.6/3"){
    val p = TreeNode("6/3")
    assert(p.evaluate() == 2.0f)
  }

  test("TreeNode.3.0+5"){
    val p = TreeNode("3.0+5")
    assert(p.evaluate() == 8.0f)
  }

  test("TreeNode.3.3*3.3"){
    val p = TreeNode("3.3*3.3")
    assert(p.evaluate() == 3.3f*3.3f)
  }

  test("TreeNode.additionFlo"){
    val p=new TreeNode("3.3+4.4")
    assert(p.evaluate() == 3.3f + 4.4f)
  }

  test("TreeNode.simpleVariable"){
    // Ici, c'est pour tester une fonction de type f(x)=x
    val p = new TreeNode("x")
    assert(p.evaluate(3.0f) == 3.0f)
  }

  test("TreeNode.3+x"){
    val p = TreeNode("3+x")
    assert(p.evaluate(10.0f) == 13.0f)
  }

  test("TreeNode.3*x"){
    val p = TreeNode("3*x")
    assert(p.evaluate(10.0f) == 30.0f)
  }

  test("TreeNode.drawing"){
    val p = new TreeNode("x")
    val o = p.evaluateWithinInterval(0.0f, 1.0f, 0.5f)
    assert(o(1)._2 == 0.5f)
    assert(o(2)._2 == 1.0f)
  }

  test("TreeNode.notASimpleOperation"){
    val p = new TreeNode("x*3.0+1.0")
    assert(p.evaluate(10.0f) == 31.0f)
  }

  test("TreeNode.negativeX"){
    val p = new TreeNode("-x")
    assert(p.evaluate(10.0f) == -10.0f)
  }

  test("TreeNode.negativeX2"){
    val p = new TreeNode("-x*3")
    assert(p.evaluate(10.0f) == -30.0f)
  }

  test("TreeNode.notASimpleOperation2"){
    val p = new TreeNode("-x*3.0-1.0")
    assert(p.evaluate(1.0f) == -4.0f)
  }


  /*
  test("TreeNode.try-priorities"){
    val p = new TreeNode("3*1+3")
    print()
  }

   */
}
