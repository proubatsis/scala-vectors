package io.github.proubatsis

import org.scalatest.{FlatSpec, Matchers}

class Mat2Spec extends FlatSpec with Matchers {
  "Mat2" should "add with another Mat2" in {
    val a = Mat2(
      1, 2,
      5, 6)

    val b = Mat2(
      1, 5,
      2, 6)

    val expected = Mat2(
      2, 7,
      7, 12)

    (a + b) shouldBe expected
  }

  it should "subtract from another Mat2" in {
    val a = Mat2(
      1, 2,
      5, 6)

    val b = Mat2(
      1, 5,
      2, 6)

    val expected = Mat2(
      0, -3,
      3, 0)

    (a - b) shouldBe expected
  }

  it should "multiply with another Mat2" in {
    val a = Mat2(
      1, 2,
      5, 6)

    val b = Mat2(
      1, 5,
      3, 7)

    val expected = Mat2(
      7, 19,
      23, 67)

    (a * b) shouldBe expected
  }

  it should "multiply with a Vec2" in {
    val a = Mat2(
      1, 2,
      5, 6)

    val b = Vec2(23, 55)

    val expected = Vec2(133, 445)

    (a * b) shouldBe expected
  }

  it should "multiply with a scalar" in {
    val a = Mat2(
      1, 2,
      5, 6)

    val b = 3

    val expected = Mat2(
      3, 6,
      15, 18)

    (a * b) shouldBe expected
  }

  it should "produce the same Mat2 when multiplied with the identity matrix" in {
    val a = Mat2(
      1, 2,
      13, 14)

    (a * Mat2.identity[Int]) shouldBe a
  }

   it should "produce a zero matrix when multiplied with a zero matrix" in {
    val a = Mat2(
      1, 2,
      5, 6)

    (a * Mat2.zero[Int]) shouldBe Mat2.zero[Int]
  }
}
