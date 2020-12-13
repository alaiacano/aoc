package com.github.alaiacano.aoc2020

import org.scalatest._
import flatspec._
import matchers._

class Dec5Test extends AnyFlatSpec with should.Matchers {
  import Dec5._
  "Dec5" should "parse B and F as binary" in {
    directionToInt("BFFFBBF", "B", "F") should be(70)
  }

  it should "parse L and R as binary" in {
    directionToInt("RRR", "R", "L") should be(7)
    directionToInt("RLL", "R", "L") should be(4)
  }

  it should "calculate seat number correctly" in {
    calculateSeat("BFFFBBFRRR") should be(567)
    calculateSeat("FFFBBBFRRR") should be(119)
    calculateSeat("BBFFBBFRLL") should be(820)
  }
}
