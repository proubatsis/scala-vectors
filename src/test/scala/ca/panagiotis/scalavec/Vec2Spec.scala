package ca.panagiotis.scalavec

import org.scalatest.{FlatSpec, Matchers}

class Vec2Spec extends FlatSpec with Matchers {
  private val spreadf = 0.1f
  private val spread = spreadf.toDouble

  "Vec2" should "have x and y components" in {
    Vec2(5, 2) shouldEqual Vec2(5, 2)
  }

  it should "add with another Vec2 object" in {
    val actual1 = Vec2(5, 2) + Vec2(3, 4)
    val expected1 = Vec2(8, 6)
    assert(actual1 === expected1)

    val actual2 = Vec2(5.2, 2.1) + Vec2(3.4, 4.5)
    val expected2 = Vec2(8.6, 6.6)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)

    val actual3 = Vec2(5.2f, 2.1f) + Vec2(3.4f, 4.5f)
    val expected3 = Vec2(8.6f, 6.6f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)
  }

  it should "subtract from another Vec2 object" in {
    assert((Vec2(5, 2) - Vec2(3, 4)) === Vec2(2, -2))

    val actual1 = Vec2(5.2, 2.1) - Vec2(3.4, 4.5)
    val expected1 = Vec2(1.8, -2.4)
    assert(actual1.x === expected1.x +- spread)
    assert(actual1.y === expected1.y +- spread)

    val actual2 = Vec2(5.2f, 2.1f) - Vec2(3.4f, 4.5f)
    val expected2 = Vec2(1.8f, -2.4f)
    assert(actual2.x === expected2.x +- spreadf)
    assert(actual2.y === expected2.y +- spreadf)
  }

  it should "be scalable" in {
    assert((Vec2(5, 2) * 3) === Vec2(15, 6))

    val actual1 = Vec2(5.3, 2.1) * 3
    val expected1 = Vec2(15.9, 6.3)
    assert(actual1.x === expected1.x +- spread)
    assert(actual1.y === expected1.y +- spread)

    val actual2 = Vec2(5.3, 2.1) * 3.0
    val expected2 = Vec2(15.9, 6.3)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)

    val actual3 = Vec2(5.3f, 2.1f) * 3
    val expected3 = Vec2(15.9f, 6.3f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)

    val actual4 = Vec2(5.3f, 2.1f) * 3.0f
    val expected4 = Vec2(15.9f, 6.3f)
    assert(actual4.x === expected4.x +- spreadf)
    assert(actual4.y === expected4.y +- spreadf)
  }

  it should "calculate the dot product" in {
    assert((Vec2(5, 2) dot Vec2(3, 7)) === 29)
    assert((Vec2(5.1, 2.4) dot Vec2(3.2, 7.7)) === 34.8 +- spread)
    assert((Vec2(5.1f, 2.4f) dot Vec2(3.2f, 7.7f)) === 34.8f +- spreadf)
  }

  it should "calculate its magnitude" in {
    assert(Vec2(5, 2).length === 5.38516 +- spread)
    assert(Vec2(5.3, 2.1).length === 5.70088 +- spread)
    assert(Vec2(5.3f, 2.1f).length === 5.70088 +- spread)
  }

   it should "be normalizable" in {
     val actual1 = Vec2(5, 2).normalize
     val expected1 = Vec2(0.92848, 0.37139)
     assert(actual1.x === expected1.x +- spread)
     assert(actual1.y === expected1.y +- spread)

     val actual2 = Vec2(5.3, 2.1).normalize
     val expected2 = Vec2(0.92968, 0.36836)
     assert(actual2.x === expected2.x +- spread)
     assert(actual2.y === expected2.y +- spread)

     val actual3 = Vec2(5.3f, 2.1f).normalize
     val expected3 = Vec2(0.92968, 0.36836)
     assert(actual3.x === expected3.x +- spread)
     assert(actual3.y === expected3.y +- spread)
  }

  it should "be convertible to a Vec2[Double] object" in {
    assert(Vec2(5, 2).asInstanceOf[Vec2[Double]] === Vec2(5.0, 2.0))
  }
}
