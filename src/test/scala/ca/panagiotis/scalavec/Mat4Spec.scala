package ca.panagiotis.scalavec

import org.scalatest.{FlatSpec, Matchers}

class Mat4Spec extends FlatSpec with Matchers {
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
}
