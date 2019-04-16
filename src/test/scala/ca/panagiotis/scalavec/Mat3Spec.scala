package ca.panagiotis.scalavec

import org.scalatest.{FlatSpec, Matchers}

class Mat3Spec extends FlatSpec with Matchers {
  "Mat3" should "add with another Mat3" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      9, 10, 11)

    val b = Mat3(
      1, 5, 9,
      2, 6, 10,
      3, 7, 11)

    val expected = Mat3(
      2, 7, 12,
      7, 12, 17,
      12, 17, 22)

    (a + b) shouldBe expected
  }

  it should "subtract from another Mat3" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      9, 10, 11)

    val b = Mat3(
      1, 5, 9,
      2, 6, 10,
      4, 8, 12)

    val expected = Mat3(
      0, -3, -6,
      3, 0, -3,
      5, 2, -1)

    (a - b) shouldBe expected
  }

  it should "multiply with another Mat3" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      9, 10, 11)

    val b = Mat3(
      1, 5, 9,
      2, 6, 10,
      3, 7, 11)

    val expected = Mat3(
      14, 38, 62,
      38, 110, 182,
      62, 182, 302)

    (a * b) shouldBe expected
  }

  it should "multiply with a Vec3" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      13, 14, 15)

    val b = Vec3(43, 23, 55)

    val expected = Vec3(254, 738, 1706)

    (a * b) shouldBe expected
  }

  it should "multiply with a scalar" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      13, 14, 15)

    val b = 3

    val expected = Mat3(
      3, 6, 9,
      15, 18, 21,
      39, 42, 45)

    (a * b) shouldBe expected
  }

  it should "produce the same Mat3 when multiplied with the identity matrix" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      13, 14, 15)

    (a * Mat3.identity[Int]) shouldBe a
  }

   it should "produce a zero matrix when multiplied with a zero matrix" in {
    val a = Mat3(
      1, 2, 3,
      5, 6, 7,
      13, 14, 15)

    (a * Mat3.zero[Int]) shouldBe Mat3.zero[Int]
  }
}
