package ca.panagiotis.scalavec

import org.scalatest.{FlatSpec, Matchers}

class Mat4Spec extends FlatSpec with Matchers {
  val spread = 0.1

  "Mat4" should "add with another Mat4" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    val b = Mat4(
      1, 5, 9, 13,
      2, 6, 10, 14,
      3, 7, 11, 15,
      4, 8, 12, 16)

    val expected = Mat4(
      2, 7, 12, 17,
      7, 12, 17, 22,
      12, 17, 22, 27,
      17, 22, 27, 32)

    (a + b) shouldBe expected
  }

  it should "subtract from another Mat4" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    val b = Mat4(
      1, 5, 9, 13,
      2, 6, 10, 14,
      3, 7, 11, 15,
      4, 8, 12, 16)

    val expected = Mat4(
      0, -3, -6, -9,
      3, 0, -3, -6,
      6, 3, 0, -3,
      9, 6, 3, 0)

    (a - b) shouldBe expected
  }

  it should "multiply with another Mat4" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    val b = Mat4(
      1, 5, 9, 13,
      2, 6, 10, 14,
      3, 7, 11, 15,
      4, 8, 12, 16)

    val expected = Mat4(
      30, 70, 110, 150,
      70, 174, 278, 382,
      110, 278, 446, 614,
      150, 382, 614, 846)

    (a * b) shouldBe expected
  }

  it should "multiply with a Vec4" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    val b = Vec4(43, 23, 55, 76)

    val expected = Vec4(558, 1346, 2134, 2922)

    (a * b) shouldBe expected
  }

  it should "multiply with a scalar" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    val b = 3

    val expected = Mat4(
      3, 6, 9, 12,
      15, 18, 21, 24,
      27, 30, 33, 36,
      39, 42, 45, 48)

    (a * b) shouldBe expected
  }

  it should "produce the same Mat4 when multiplied with the identity matrix" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    (a * Mat4.identity[Int]) shouldBe a
  }

   it should "produce a zero matrix when multipled with a zero matrix" in {
    val a = Mat4(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16)

    (a * Mat4.zero[Int]) shouldBe Mat4.zero[Int]
  }

  it should "scale a Vec4" in {
    val m = Mat4.createScale(5, 2, 3)
    val v = Vec4(1, 2, 3, 1)
    val actual = m * v
    val expected = Vec4(5, 4, 9, 1)

    actual shouldBe expected
  }

  it should "rotate a Vec4 around the x-axis" in {
    val m = Mat4.createRotationX(Math.PI / 2.0)
    val v = Vec4(5.0, 3.0, 0.0, 0.0)
    val actual = m * v
    val expected = Vec4(5.0, 0.0, 3.0, 0.0)

    assert(actual.x === expected.x +- spread)
    assert(actual.y === expected.y +- spread)
    assert(actual.z === expected.z +- spread)
    assert(actual.w === expected.w +- spread)
  }

   it should "rotate a Vec4 around the y-axis" in {
    val m = Mat4.createRotationY(Math.PI / 2.0)
    val v = Vec4(3.0, 5.0, 0.0, 1.0)
    val actual = m * v
    val expected = Vec4(0.0, 5.0, -3.0, 1.0)

    assert(actual.x === expected.x +- spread)
    assert(actual.y === expected.y +- spread)
    assert(actual.z === expected.z +- spread)
    assert(actual.w === expected.w +- spread)
  }

  it should "rotate a Vec4 around the z-axis" in {
    val m = Mat4.createRotationZ(Math.PI / 2.0)
    val v = Vec4(0.0, 3.0, 5.0, 0.0)
    val actual = m * v
    val expected = Vec4(-3.0, 0.0, 5.0, 0.0)

    assert(actual.x === expected.x +- spread)
    assert(actual.y === expected.y +- spread)
    assert(actual.z === expected.z +- spread)
    assert(actual.w === expected.w +- spread)
  }

  it should "translate a position vector" in {
    val m = Mat4.createTranslation(5, -2, 3)
    val v = Vec4(1, 2, 3, 1)
    val actual = m * v
    val expected = Vec4(6, 0, 6, 1)

    actual shouldBe expected
  }

   it should "not translate a direction vector" in {
    val m = Mat4.createTranslation(5, -2, 3)
    val v = Vec4(1, 2, 3, 0)
    val actual = m * v
    val expected = v

    actual shouldBe expected
  }

  it should "apply a combined transformation" in {
    val scale = Mat4.createScale(5.0, 2.0, 3.0)
    val rotate = Mat4.createRotationZ(Math.PI / 2.0)
    val translate = Mat4.createTranslation(5.0, -2.0, 3.0)
    val m = translate * (rotate * scale)

    val pos = Vec4(5.0, 0.0, 7.0, 1.0)
    val actualPos = m * pos
    val expectedPos = Vec4(5.0, 23.0, 24.0, 1.0)

    assert(actualPos.x === expectedPos.x +- spread)
    assert(actualPos.y === expectedPos.y +- spread)
    assert(actualPos.z === expectedPos.z +- spread)
    assert(actualPos.w === expectedPos.w +- spread)

    val dir = Vec4(5.0, 0.0, 7.0, 0.0)
    val actualDir = m * dir
    val expectedDir = Vec4(0.0, 25.0, 21.0, 0.0)

    assert(actualPos.x === expectedPos.x +- spread)
    assert(actualPos.y === expectedPos.y +- spread)
    assert(actualPos.z === expectedPos.z +- spread)
    assert(actualPos.w === expectedPos.w +- spread)
  }
}
