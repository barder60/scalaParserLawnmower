package example

import exceptions.WrongUserInput
import org.scalatest.funsuite.AnyFunSuite
import progfun.{Board, Parser}

class ParserSpec extends AnyFunSuite {

  //parse boardd oit vérifier
  //2 paramètres
  //qu'ils sont bien int
  //

  //  test("parse string should call parse board") {
  //    val parser = new Parser("5,5;1,2,E;DDD;1,3,S;GAGAGA;")
  //    parser.parseString()
  //  }

  test(
    "parse board should throw incorrect parameter exception when not 2 digits"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")

    val result: Either[WrongUserInput, Board] = parser.parseBoard("43")
    assert(Left(WrongUserInput("Wrong board parameter")) === result)
  }

  test("parse board should throw incorrect parameter exception when not int") {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")

    val result: Either[WrongUserInput, Board] =
      parser.parseBoard("zoulou, zoulou")
    assert(Left(WrongUserInput("Wrong board parameter")) === result)
  }

  test("parse board should return board") {
    val parser = new Parser("5,5;1,2,E;DDD;1,3,S;GAGAGA;")
    val expected = Board(5, 5)
    val board: Either[WrongUserInput, Board] = parser.parseBoard("5,5")
    val result = board.getOrElse(Board(0, 0))
    assert(result.limitX() === expected.limitX())
    assert(result.limitY() === expected.limitY())
  }

}
