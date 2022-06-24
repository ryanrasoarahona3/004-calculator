import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestTreeNode2Simplify extends AnyFunSuite{
  test("Simplify.basic"){
    val p = TreeNode2("3.0")
    val q = p.simplify()
    assert(q.expression == "3.0")
  }

  test("Simplify.x"){
    val p = TreeNode2("x")
    val q = p.simplify()
    assert(q.expression == "x")
  }

  test("Simplify.constant"){
    val p = TreeNode2("3.0+3.0")
    val q = p.simplify()
    assert(q.expression == "6.0")
  }

  test("Simplify.nestedConstant"){
    val p = TreeNode2("(3.0+3.0)*x")
    val q = p.simplify()
    assert(q.expression == "6.0*x")
  }

  test("Simplify.nestedWithConstantAndX"){
    val p = TreeNode2("1.0+1.0+x")
    val q = p.simplify()
    print()
  }

  /*
  // TODO: À faire plus tard
  test("Simplify.x+x"){
    val p = TreeNode2("x+x")
    val q = p.simplify()
    assert(q.expression == "2.0*x")
  }

  test("Simplify.2*x+x"){
    val p = TreeNode2("2*x+x")
    val q = p.simplify()
    assert(q.expression == "3.0*x")
  }
   */
}
