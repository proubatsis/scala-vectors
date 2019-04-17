package ca.panagiotis.scalavec

import org.scalatest.{FlatSpec, Matchers}

class Vec3Spec extends FlatSpec with Matchers {
  private val spreadf = 0.1f
  private val spread = spreadf.toDouble

  "Vec3" should "have x and y components" in {
    Vec3(5, 2, 7) shouldEqual Vec3(5, 2, 7)
  }

  it should "add with another Vec3 object" in {
    val actual1 = Vec3(5, 2, 7) + Vec3(3, 4, 1)
    val expected1 = Vec3(8, 6, 8)
    assert(actual1 === expected1)

    val actual2 = Vec3(5.2, 2.1, 7.75) + Vec3(3.4, 4.5, 1.25)
    val expected2 = Vec3(8.6, 6.6, 9.0)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)
    assert(actual2.z === expected2.z +- spread)

    val actual3 = Vec3(5.2f, 2.1f, 7.75f) + Vec3(3.4f, 4.5f, 1.25f)
    val expected3 = Vec3(8.6f, 6.6f, 9.0f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)
    assert(actual3.z === expected3.z +- spreadf)
  }

  it should "subtract from another Vec3 object" in {
    assert((Vec3(5, 2, 7) - Vec3(3, 4, 1)) === Vec3(2, -2, 6))

    val actual1 = Vec3(5.2, 2.1, 7.75) - Vec3(3.4, 4.5, 1.25)
    val expected1 = Vec3(1.8, -2.4, 6.5)
    assert(actual1.x === expected1.x +- spread)
    assert(actual1.y === expected1.y +- spread)
    assert(actual1.z === expected1.z +- spread)

    val actual2 = Vec3(5.2f, 2.1f, 7.75f) - Vec3(3.4f, 4.5f, 1.25f)
    val expected2 = Vec3(1.8f, -2.4f, 6.5f)
    assert(actual2.x === expected2.x +- spreadf)
    assert(actual2.y === expected2.y +- spreadf)
    assert(actual2.z === expected2.z +- spreadf)
  }

  it should "be scalable" in {
    assert((Vec3(5, 2, 7) * 3) === Vec3(15, 6, 21))

    val actual1 = Vec3(5.3, 2.1, 7.75) * 3
    val expected1 = Vec3(15.9, 6.3, 23.25)
    assert(actual1.x === expected1.x +- spread)
    assert(actual1.y === expected1.y +- spread)
    assert(actual1.z === expected1.z +- spread)

    val actual2 = Vec3(5.3, 2.1, 7.75) * 3.0
    val expected2 = Vec3(15.9, 6.3, 23.25)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)
    assert(actual2.z === expected2.z +- spread)

    val actual3 = Vec3(5.3f, 2.1f, 7.75f) * 3
    val expected3 = Vec3(15.9f, 6.3f, 23.25f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)
    assert(actual3.z === expected3.z +- spreadf)

    val actual4 = Vec3(5.3f, 2.1f, 7.75f) * 3.0f
    val expected4 = Vec3(15.9f, 6.3f, 23.25f)
    assert(actual4.x === expected4.x +- spreadf)
    assert(actual4.y === expected4.y +- spreadf)
    assert(actual4.z === expected4.z +- spreadf)
  }

  it should "calculate the dot product" in {
    assert((Vec3(5, 2, 7) dot Vec3(3, 7, 1)) === 36)
    assert((Vec3(5.1, 2.4, 7.75) dot Vec3(3.2, 7.7, 1.25)) === 44.4875 +- spread)
    assert((Vec3(5.1f, 2.4f, 7.75f) dot Vec3(3.2f, 7.7f, 1.25f)) === 44.4875f +- spreadf)
  }

  it should "calculate its magnitude" in {
    assert(Vec3(5, 2, 7).length === 8.83176 +- spread)
    assert(Vec3(5.3, 2.1, 7.75).length === 9.62094 +- spread)
    assert(Vec3(5.3f, 2.1f, 7.75f).length === 9.62094 +- spread)
  }

   it should "be normalizable" in {
     val actual1 = Vec3(5, 2, 7).normalize
     val expected1 = Vec3(0.56614, 0.22646, 0.79259)
     assert(actual1.x === expected1.x +- spread)
     assert(actual1.y === expected1.y +- spread)
     assert(actual1.z === expected1.z +- spread)

     val actual2 = Vec3(5.3, 2.1, 7.75).normalize
     val expected2 = Vec3(0.55088, 0.21827, 0.80553)
     assert(actual2.x === expected2.x +- spread)
     assert(actual2.y === expected2.y +- spread)
     assert(actual2.z === expected2.z +- spread)

     val actual3 = Vec3(5.3f, 2.1f, 7.75f).normalize
     val expected3 = Vec3(0.55088, 0.21827, 0.80553)
     assert(actual3.x === expected3.x +- spread)
     assert(actual3.y === expected3.y +- spread)
     assert(actual3.z === expected3.z +- spread)
  }

  it should "calculate the cross product" in {
    val actual1 = Vec3(5, 2, 7) cross Vec3(3, 4, 1)
    val expected1 = Vec3(-26, 16, 14)
    assert(actual1 === expected1)
    val v4A = Vec4(1, 2, 3, 1)
    val actualA = Vec3.fromVec4(v4A)
    val expectedA = Vec3(1, 2, 3)
    val actual2 = Vec3(5.3, 2.1, 7.75) cross Vec3(3.4, 4.5, 1.25)
    val expected2 = Vec3(-32.25, 19.725, 16.71)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)
    assert(actual2.z === expected2.z +- spread)

    val actual3 = Vec3(5.3f, 2.1f, 7.75f) cross Vec3(3.4f, 4.5f, 1.25f)
    val expected3 = Vec3(-32.25f, 19.725f, 16.71f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)
    assert(actual3.z === expected3.z +- spreadf)
  }

  it should "be convertible to a Vec3[Double] object" in {
    assert(Vec3(5, 2, 7).asInstanceOf[Vec3[Double]] === Vec3(5.0, 2.0, 7.0))
  }

  it should "instantiate from a Vec2" in {
    val v2 = Vec2(5, 2)

    val actual = Vec3.fromVec2(v2, 9)
    val expected = Vec3(5, 2, 9)
    assert(actual === expected)
  }

  it should "instantiate from Vec4" in {
    val v4A = Vec4(1, 2, 3, 1)
    val actualA = Vec3.fromVec4(v4A)
    val expectedA = Vec3(1, 2, 3)

    assert(actualA === expectedA)

    val v4B = Vec4(1, 2, 3, 0)
    val actualB = Vec3.fromVec4(v4B)
    val expectedB = Vec3(1, 2, 3)

    assert(actualB === expectedB)
  }
}
