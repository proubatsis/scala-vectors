package io.github.proubatsis

import org.scalatest.{FlatSpec, Matchers}

class Vec4Spec extends FlatSpec with Matchers {
  private val spreadf = 0.1f
  private val spread = spreadf.toDouble

  "Vec4" should "have x, y, z, and w components" in {
    Vec4(5, 2, 7, 1) shouldEqual Vec4(5, 2, 7, 1)
  }

  it should "add with another Vec4 object" in {
    val actual1 = Vec4(5, 2, 7, 1) + Vec4(3, 4, 1, 8)
    val expected1 = Vec4(8, 6, 8, 9)
    assert(actual1 === expected1)

    val actual2 = Vec4(5.2, 2.1, 7.75, 1.0) + Vec4(3.4, 4.5, 1.25, 8.0)
    val expected2 = Vec4(8.6, 6.6, 9.0, 9.0)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)
    assert(actual2.z === expected2.z +- spread)
    assert(actual2.w === expected2.w +- spread)

    val actual3 = Vec4(5.2f, 2.1f, 7.75f, 1.0f) + Vec4(3.4f, 4.5f, 1.25f, 8.0f)
    val expected3 = Vec4(8.6f, 6.6f, 9.0f, 9.0f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)
    assert(actual3.z === expected3.z +- spreadf)
    assert(actual3.w === expected3.w +- spreadf)
  }

  it should "subtract from another Vec4 object" in {
    assert((Vec4(5, 2, 7, 1) - Vec4(3, 4, 1, 8)) === Vec4(2, -2, 6, -7))

    val actual1 = Vec4(5.2, 2.1, 7.75, 1.0) - Vec4(3.4, 4.5, 1.25, 8.0)
    val expected1 = Vec4(1.8, -2.4, 6.5, -7.0)
    assert(actual1.x === expected1.x +- spread)
    assert(actual1.y === expected1.y +- spread)
    assert(actual1.z === expected1.z +- spread)
    assert(actual1.w === expected1.w +- spread)

    val actual2 = Vec4(5.2f, 2.1f, 7.75f, 1.0) - Vec4(3.4f, 4.5f, 1.25f, 8.0)
    val expected2 = Vec4(1.8f, -2.4f, 6.5f, -7.0)
    assert(actual2.x === expected2.x +- spreadf)
    assert(actual2.y === expected2.y +- spreadf)
    assert(actual2.z === expected2.z +- spreadf)
    assert(actual2.w === expected2.w +- spreadf)
  }

  it should "be scalable" in {
    assert((Vec4(5, 2, 7, 1) * 3) === Vec4(15, 6, 21, 3))

    val actual1 = Vec4(5.3, 2.1, 7.75, 1.0) * 3
    val expected1 = Vec4(15.9, 6.3, 23.25, 3.0)
    assert(actual1.x === expected1.x +- spread)
    assert(actual1.y === expected1.y +- spread)
    assert(actual1.z === expected1.z +- spread)
    assert(actual1.w === expected1.w +- spread)

    val actual2 = Vec4(5.3, 2.1, 7.75, 1.0) * 3.0
    val expected2 = Vec4(15.9, 6.3, 23.25, 3.0)
    assert(actual2.x === expected2.x +- spread)
    assert(actual2.y === expected2.y +- spread)
    assert(actual2.z === expected2.z +- spread)
    assert(actual2.w === expected2.w +- spread)

    val actual3 = Vec4(5.3f, 2.1f, 7.75f, 1.0f) * 3
    val expected3 = Vec4(15.9f, 6.3f, 23.25f, 3.0f)
    assert(actual3.x === expected3.x +- spreadf)
    assert(actual3.y === expected3.y +- spreadf)
    assert(actual3.z === expected3.z +- spreadf)
    assert(actual3.w === expected3.w +- spreadf)

    val actual4 = Vec4(5.3f, 2.1f, 7.75f, 1.0f) * 3.0f
    val expected4 = Vec4(15.9f, 6.3f, 23.25f, 3.0f)
    assert(actual4.x === expected4.x +- spreadf)
    assert(actual4.y === expected4.y +- spreadf)
    assert(actual4.z === expected4.z +- spreadf)
    assert(actual4.w === expected4.w +- spreadf)
  }

  it should "calculate the dot product" in {
    assert((Vec4(5, 2, 7, 1) dot Vec4(3, 7, 1, 8)) === 44)
    assert((Vec4(5.1, 2.4, 7.75, 1.0) dot Vec4(3.2, 7.7, 1.25, 8.0)) === 52.4875 +- spread)
    assert((Vec4(5.1f, 2.4f, 7.75f, 1.0f) dot Vec4(3.2f, 7.7f, 1.25f, 8.0f)) === 52.4875f +- spreadf)
  }

  it should "calculate its magnitude" in {
    assert(Vec4(5, 2, 7, 1).length === 8.88819 +- spread)
    assert(Vec4(5.3, 2.1, 7.75, 1.0).length === 9.67277 +- spread)
    assert(Vec4(5.3f, 2.1f, 7.75f, 1.0f).length === 9.67277 +- spread)
  }

   it should "be normalizable" in {
     val actual1 = Vec4(5, 2, 7, 1).normalize
     val expected1 = Vec4(0.56254, 0.22502, 0.78756, 0.11251)
     assert(actual1.x === expected1.x +- spread)
     assert(actual1.y === expected1.y +- spread)
     assert(actual1.z === expected1.z +- spread)
     assert(actual1.w === expected1.w +- spread)

     val actual2 = Vec4(5.3, 2.1, 7.75, 1.0).normalize
     val expected2 = Vec4(0.54792, 0.2171, 0.80122, 0.10338)
     assert(actual2.x === expected2.x +- spread)
     assert(actual2.y === expected2.y +- spread)
     assert(actual2.z === expected2.z +- spread)
     assert(actual2.w === expected2.w +- spread)

     val actual3 = Vec4(5.3f, 2.1f, 7.75f, 1.0f).normalize
     val expected3 = Vec4(0.54792, 0.2171, 0.80122, 0.10338)
     assert(actual3.x === expected3.x +- spreadf)
     assert(actual3.y === expected3.y +- spreadf)
     assert(actual3.z === expected3.z +- spreadf)
     assert(actual3.w === expected3.w +- spreadf)
  }

  it should "be convertible to a Vec4[Double] object" in {
    assert(Vec4(5, 2, 7, 1).asInstanceOf[Vec4[Double]] === Vec4(5.0, 2.0, 7.0, 1.0))
  }

  it should "instantiate a position vector from a Vec3" in {
    val v3 = Vec3(5, 2, 8)
    val actual = Vec4.fromVec3(v3, true)
    val expected = Vec4(5, 2, 8, 1)
    assert(actual === expected)
  }

   it should "instantiate a direction vector from a Vec3" in {
    val v3 = Vec3(5, 2, 8)
    val actual = Vec4.fromVec3(v3, false)
    val expected = Vec4(5, 2, 8, 0)
    assert(actual === expected)
  }
}
