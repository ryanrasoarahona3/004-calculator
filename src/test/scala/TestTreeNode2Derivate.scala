import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestTreeNode2Derivate extends AnyFunSuite{

  test("TreeNode2.derivateOfConstant"){
    val p = new TreeNode2("3.0")
    val q = p.derivate()
    assert(q.evaluate() == 0)
  }

  test("TreeNode2.derivateOfX"){
    val p = TreeNode2("x")
    val q = p.derivate()
    assert(q.evaluate() == 1.0)
  }

  test("TreeNode2.derivateOfPlaceHolder"){
    val p = TreeNode2("_", Array("x"))
    val q = p.derivate()
    assert(q.evaluate() == 1.0)
  }

  test("TreeNode2.derivate.simpleAddition"){
    val p = TreeNode2("x+3")
    val q = p.derivate()
    assert(q.evaluate() == 1.0)
  }

  test("TreeNode2.derivate.simpleAddition2"){
    val p = TreeNode2("x-4")
    val q = p.derivate()
    assert(q.evaluate() == 1.0)
  }

  test("TreeNode2.derivate.multiplication"){
    val p = TreeNode2("3*x")
    val q = p.derivate()
    assert(q.evaluate() == 3.0)
  }

  test("TreeNode2.derivate.division"){
    val p = TreeNode2("3/x")
    val q = p.derivate()
    assert(q.evaluate(2) == (-3.0f/4))
  }

  test("TreeNode2.derivate.square"){
    val p = TreeNode2("x*x")
    val q = p.derivate()
    assert(q.evaluate(2) == (4))
  }
}
