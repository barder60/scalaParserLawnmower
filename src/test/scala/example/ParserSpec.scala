package example

import exceptions.WrongUserInput
import org.scalatest.funsuite.AnyFunSuite
import progfun.{Board, Lawnmower, Parser}

class ParserSpec extends AnyFunSuite {

  test(
    "parse board should throw incorrect parameter exception when not 2 digits"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")

    val result: Either[WrongUserInput, Board] = parser.parseBoard("43")
    assert(result === Left(WrongUserInput("Wrong board parameter")))
  }

  test("parse board should throw incorrect parameter exception when not int") {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")

    val result: Either[WrongUserInput, Board] =
      parser.parseBoard("zoulou, zoulou")
    assert(result === Left(WrongUserInput("Wrong board parameter")))
  }

  test("parse board should return board") {
    val parser = new Parser("5,5;1,2,E;DDD;1,3,S;GAGAGA;")
    val expected = Board(5, 5)
    val board: Either[WrongUserInput, Board] = parser.parseBoard("5,5")
    val result = board.getOrElse(Board(0, 0))
    assert(result.limitX() === expected.limitX())
    assert(result.limitY() === expected.limitY())
  }

  //parse tondeuse
  //doit vérifier qu'(on a bien 4 inputs par tondeuse
  //que y'a bien 3 inputs dans chaque position / direction

  test("parse lawnmowers should return wrong input if size not pair") {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("1,2,3", "DDD", "3,4,3")
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(wrongParams, List())
    assert(result === Left(WrongUserInput("Wrong lawnmowers parameter number")))
  }

  test(
    "parse lawnmowers should return wrong input if there isnt 3 param for position and direction"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("1,2", "DDD", "3,4,3", "ADG")
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(wrongParams, List())
    assert(
      result === Left(WrongUserInput("Wrong lawnmowers position / orientation"))
    )
  }

  test(
    "parse lawnmowers should return wrong action if action not known"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("1,2", "XDD", "3,4,3", "ADG")
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(wrongParams, List())
    assert(
      result === Left(WrongUserInput("Wrong action : X"))
    )
  }

}
