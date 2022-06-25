import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestGetExpression extends AnyFunSuite{
  test("Expr.constant"){
    val p = TreeNode2("3.0")
    assert(p.getExpression() == "3.0")
  }

  test("Expr.variablex"){
    val p = TreeNode2("x")
    assert(p.getExpression() == "x")
  }

  test("Expr.3+3"){
    val p = TreeNode2("3.0+3.0")
    assert(p.getExpression() == "3.0+3.0")
  }

  test("Expr.3*3"){
    val p = TreeNode2("3.0*3.0")
    assert(p.getExpression() == "3.0*3.0")
  }

  test("Expr.(3+3)*2"){
    val p = TreeNode2("(3.0+3.0)*2.0")
    assert(p.getExpression() == "(3.0+3.0)*2.0")
  }

  test("Expr.(3+3+3)"){
    val p = TreeNode2("3.0+3.0+3.0")
    assert(p.getExpression() == "3.0+3.0+3.0")
  }

  test("Expr.sin(x)"){
    val p = TreeNode2("sin(x)")
    assert(p.getExpression() == "sin(x)")
  }

  test("Expr.2.0*sin(x+2.0)"){
    val p = TreeNode2("2.0*sin(x+2.0)")
    assert(p.getExpression() == "2.0*sin(x+2.0)")
  }

  test("Expr.derivate"){
    val p = TreeNode2("sin(x)")
    val q = p.derivate()
    assert(q.getExpression() == "1.0*cos(x)")
  }
}
