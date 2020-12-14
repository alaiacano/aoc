package com.github.alaiacano.aoc2020
import com.twitter.algebird.Operators._

import org.scalatest._
import flatspec._
import matchers._

class Dec7Test extends AnyFlatSpec with should.Matchers {
  import Dec7._

  "Dec7" should "parse lines with two bags correctly" in {
    val expected = Map("bright white" -> Seq(Container("light red", 1)), "muted yellow" -> Seq(Container("light red", 2)))
    parseLine("light red bags contain 1 bright white bag, 2 muted yellow bags.") should be(expected)
  }

  it should "parse lines with 1 bag correctly" in {
    val expected = Map("shiny gold" -> Seq(Container("bright white", 1)))
    parseLine("bright white bags contain 1 shiny gold bag.") should be(expected)
  }

  it should "parse lines with many bags correctly" in {
    val expected = Map(
      "mirrored blue" -> Seq(Container("plaid bronze", 4)),
      "wavy green" -> Seq(Container("plaid bronze", 4)),
      "faded magenta" -> Seq(Container("plaid bronze", 4)),
      "plaid olive" -> Seq(Container("plaid bronze", 5))
      )
    val actual = parseLine("plaid bronze bags contain 4 wavy green bags, 4 mirrored blue bags, 4 faded magenta bags, 5 plaid olive bags.")
    actual should be(expected)
  }

  it should "combine rows properly" in {
    val expected = Map(
      "bright white" -> Seq(Container("light red", 1)), 
      "muted yellow" -> Seq(Container("light red", 2)),
      "shiny gold" -> Seq(Container("bright white", 1), Container("hot pink", 2)))

    val p1 = parseLine("light red bags contain 1 bright white bag, 2 muted yellow bags.")
    val p2 = parseLine("bright white bags contain 1 shiny gold bag.")
    val p3 = parseLine("hot pink bags contain 2 shiny gold bags.")
    Seq(p1 + p2 + p3).reduce(_ + _) should be(expected)
  }

  // it should "return empty map on bad parse" in {
  //   parseLine("blahhhhh") should be(Map.empty)
  // }
}