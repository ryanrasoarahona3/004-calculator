import org.calculator.TreeNode2
import org.scalatest.funsuite.AnyFunSuite

class TestSimplification extends AnyFunSuite{

  test("Simplify.scalarOperation"){
    val p = TreeNode2("2.0+3.0")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "5.0")
  }

  test("Simplify.x"){
    val p = TreeNode2("x")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "x")
  }

  test("Simplify.scalarOperationWithX"){
    val p = TreeNode2("(1.0+1.0)*x")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "2.0*x")
  }

  test("Simplify.sortScalarAndX"){
    val p = TreeNode2("1.0+x")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "x+1.0")
  }

  test("Simplify.AdditionWithinNested"){
    val p = TreeNode2("x+1.0+1.0")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "x+2.0")
  }

  test("Simplify.MultiplicationByZero"){
    val p = TreeNode2("0.0*x")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "0.0")
  }

  test("Simplify.MultiplicationWithOne"){
    val p = TreeNode2("1.0*x")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "x")
  }

  test("Simplify.AdditionWithZero"){
    val p = TreeNode2("x+0.0")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "x")
  }

  test("Simplification.derivative3*x"){
    val p = TreeNode2("3.0*x")
    val q = p.derivate()
    val r = q.getSimplifiedLoop()
    print()
  }

  test("Simplification.x+x"){
    val p = TreeNode2("x+x")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "2.0*x")
  }

  /*
  test("Simplify.sinCosTest"){
    val p = TreeNode2("cos(x)")
    val q = p.derivate()
    print()
  }
   */

  test("Simplification.-1.0*2.0*2.0*x"){
    val p = TreeNode2("sin(x+x)")
    assert(p.getSimplifiedLoop().getExpression() == "sin(2.0*x)")
  }

  test("Simplification.cos(3.0*x)"){
    val p = TreeNode2("1.0*3.0*sin(3.0*x)")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "3.0*sin(3.0*x)")
  }

  test("Simplification.cos(x+x)derivate"){
    val p = TreeNode2("cos(2.0*x)")
    val q = p.derivate()
    val r = q.getSimplifiedLoop()
    assert(r.getExpression() == "-2.0*sin(2.0*x)")
    print()
  }

  /*
  test("Simplification.TreeNode2(\"2.0*1.0+0.0*x\")"){
    val p = TreeNode2("-1.0*((2.0*1.0+0.0*x)*sin(x))")
    print()
  }
   */

  test("Simplification.2.0*sin(2.0*x)"){
    val p = TreeNode2("2.0*sin(2.0*x)")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "2.0*sin(2.0*x)")
  }

  test("Simplification.-1.0*2.0*sin(2.0*x)"){
    val p = TreeNode2("(-1.0)*(2.0*sin(2.0+x))")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "-2.0*sin(x+2.0)")
  }

  test("Simplification.cos(x)+-2.0*sin(2.0*x)"){
    val p = TreeNode2("x+(-1.0*sin(x))")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "x-sin(x)")
  }

  test("Simplification.sin(x)+cos(x)"){
    val p = TreeNode2("sin(x)+cos(x)")
    val q = p.derivate().getSimplifiedLoop()
    assert(q.getExpression() == "cos(x)-sin(x)")
  }

  test("Simplification.2.0*x*1.0+2.0*x"){
    val p = TreeNode2("2.0*x*2.0")
    val q = p.getSimplifiedLoop()
    assert(q.getExpression() == "4.0*x")
  }

  test("Simplification.loopTest"){
    val p = TreeNode2("x*x")
    val q = p.derivate()
    val r = q.getSimplifiedLoop()
    assert(r.getExpression() == "2.0*x")
  }
}
