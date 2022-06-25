import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestSimplification extends AnyFunSuite{

  test("Simplify.scalarOperation"){
    val p = TreeNode2("2.0+3.0")
    val q = p.getSimplified()
    assert(q.getExpression() == "5.0")
  }

  test("Simplify.x"){
    val p = TreeNode2("x")
    val q = p.getSimplified()
    assert(q.getExpression() == "x")
  }

  test("Simplify.scalarOperationWithX"){
    val p = TreeNode2("(1.0+1.0)*x")
    val q = p.getSimplified()
    assert(q.getExpression() == "2.0*x")
  }

  test("Simplify.sortScalarAndX"){
    val p = TreeNode2("1.0+x")
    val q = p.getSimplified()
    assert(q.getExpression() == "x+1.0")
  }

  test("Simplify.AdditionWithinNested"){
    val p = TreeNode2("x+1.0+1.0")
    val q = p.getSimplified()
    assert(q.getExpression() == "x+2.0")
  }

  test("Simplify.MultiplicationByZero"){
    val p = TreeNode2("0.0*x")
    val q = p.getSimplified()
    assert(q.getExpression() == "0.0")
  }

  /*
  test("Simplify.sinCosTest"){
    val p = TreeNode2("cos(x)")
    val q = p.derivate()
    print()
  }
   */
}
