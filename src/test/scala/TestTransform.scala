
import org.calculator.Transform
import org.scalatest.funsuite.AnyFunSuite


class TestTransform extends AnyFunSuite {
  test("transform.test-1"){
    val t = new Transform(200, 200, 10)
    assert(t.apply(0.0f, 0.0f) == (200.0f, 200.0f))
  }

  test("transform.test-2"){
    val t = new Transform(200, 200, 10)
    assert(t.apply(1.0f, 1.0f) == (210.0f, 210.0f))
  }

  test("transform.test-3"){
    val t = new Transform(200, 200, 10)
    assert(t.apply(-1.0f, -1.0f) == (190.0f, 190.0f))
  }
}
