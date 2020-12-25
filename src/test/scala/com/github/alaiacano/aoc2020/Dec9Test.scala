package com.github.alaiacano.aoc2020

import org.scalatest._
import flatspec._
import matchers._

class Dec9Test extends AnyFlatSpec with should.Matchers {
  import Dec9._

  "Dec9" should "check that a number is valid" in {
    val preamble = (1l to 25l).toSet
    isValid(26, preamble) should be(true)
    isValid(49, preamble) should be(true)
    isValid(100, preamble) should be(false)
    isValid(50, preamble) should be(false)
  }
}