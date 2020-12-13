package com.github.alaiacano.aoc2020

import org.scalatest._
import flatspec._
import matchers._

import Dec4._

class Dec4Test extends AnyFlatSpec with should.Matchers {
  "Dec4" should "validate height strings properly" in {
    validHeightStr("100cm") should be(true)
    validHeightStr("20in") should be(true)
    validHeightStr("1in") should be(false)
    validHeightStr("100xa") should be(false)
  }

  it should "validate birth years correctly" in {
    validate("byr", "1926") should be(true)
    validate("byr", "1826") should be(false)
    validate("byr", "2003") should be(false)
  }

  it should "mark the invalid examples as invalid" in {
    validPassport(
      """eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926""",
      true
    ) should be(false)

    validPassport(
      """iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946""",
      true
    ) should be(false)

    validPassport(
      """hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277""",
      true
    ) should be(false)

    validPassport(
      """hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007""",
      true
    ) should be(false)

  }

  it should "validate pid" in {
    validate("pid", "087499704") should be(true)
    validate("pid", "12345678x") should be(false)
    validate("pid", "123") should be(false)
    validate("pid", "1234567890") should be(false)
  }
  it should "validate eyr" in {
    validate("eyr", "2030") should be(true)
    validate("eyr", "2031")should be(false)
    validate("eyr", "02030") should be(false)
  }
  it should "validate hcl" in {
    validate("hcl", "#623a2f") should be(true)
    validate("hcl", "#!23a2f") should be(false)
    validate("hcl", "623a2f") should be(false)
    validate("hcl", "#6a2f") should be(false)
    validate("hcl", "#623a2fffff") should be(false)
  }
  it should "validate hgt" in {
    validate("hgt", "74in") should be(true)
    validate("hgt", "190cm") should be(true)
    validate("hgt", "58in") should be(false)
    validate("hgt", "77in") should be(false)
    validate("hgt", "74mm") should be(false)
    validate("hgt", "149cm") should be(false)
    validate("hgt", "194cm") should be(false)
  }
  it should "validate ecl" in {
    validate("ecl", "grn") should be(true)
    validate("ecl", "fdsasdf") should be(false)
  }
  it should "validate iyr" in {
    validate("iyr", "2012") should be(true)
    validate("iyr", "2010") should be(true)
    validate("iyr", "2020") should be(true)
    validate("iyr", "2021") should be(false)
    validate("iyr", "2009") should be(false)
  }
  it should "validate byr" in {
    validate("byr", "1980") should be(true)
    validate("byr", "1920") should be(true)
    validate("byr", "2002") should be(true)
    validate("byr", "2003") should be(false)
    validate("byr", "1919") should be(false)
  }
  it should "parse a valid passport" in {
    val expected = collection.mutable.Map(
      "pid" -> "087499704",
      "hgt" -> "74in",
      "ecl" -> "grn",
      "iyr" -> "2012",
      "eyr" -> "2030",
      "byr" -> "1980",
      "hcl" -> "#623a2f"
    )
    parseSinglePassport("""pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f""") should be(expected)
  }

  it should "mark the valid examples as valid" in {
    validPassport(
      """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f""",
      true
    ) should be(true)

    validPassport(
      """eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm""",
      true
    ) should be(true)

    validPassport(
      """hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022""",
      true
    ) should be(true)

    validPassport(
      """iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""",
      true
    ) should be(true)
  }
}
