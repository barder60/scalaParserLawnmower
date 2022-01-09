package example

import exceptions.WrongUserInput
import org.scalatest.funsuite.AnyFunSuite
import progfun.{Action, Board, Orientation, Lawnmower, Parser, Result}

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

  test(
    "parse board should throw incorrect parameter exception when not negative parameters"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")

    val result: Either[WrongUserInput, Board] =
      parser.parseBoard("40, -1")
    assert(result === Left(WrongUserInput("Wrong board parameter")))
  }

  test("parse board should return board") {
    val parser = new Parser("5,5;1,2,E;DDD;1,3,S;GAGAGA;")
    val expected = Board(5, 5)
    val board: Either[WrongUserInput, Board] = parser.parseBoard("5,5")
    val result = board.getOrElse(Board(0, 0))
    assert(result.limitX === expected.limitX)
    assert(result.limitY === expected.limitY)
  }

  test("parse lawnmowers should return wrong input if size not pair") {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val board = Board(7, 7)
    val wrongParams = List("1,2,3", "DDD", "3,4,3")
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(result === Left(WrongUserInput("Wrong lawnmowers parameter number")))
  }

  test(
    "parse lawnmowers should return wrong input if there isnt 3 param for position and orientation"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("1,2", "DDD", "3,4,3", "ADG")
    val board = Board(7, 7)
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(
      result === Left(WrongUserInput("Wrong lawnmowers position / orientation"))
    )
  }

  test(
    "parse lawnmowers should return wrong action if action not known"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("1,2,E", "XDD", "3,4,3", "ADG")
    val board = Board(7, 7)
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(
      result === Left(WrongUserInput("Wrong action : X"))
    )
  }

  test(
    "parse lawnmowers should return wrong orientation if orientation not known"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("1,2,3", "DDD", "3,4,3", "ADG")
    val board = Board(7, 7)
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(
      result === Left(WrongUserInput("Wrong orientation : 3"))
    )
  }

  test(
    "parse lawnmowers should return wrong pos if posa re not numbers"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("zaza,zozo,E", "DDD", "3,4,3", "ADG")
    val board = Board(7, 7)
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(
      result === Left(WrongUserInput("Wrong position : zaza"))
    )
  }

  test(
    "parse lawnmowers should return all lawnmowers"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val goodParams = List("1,2,E", "DAGA", "3,4,N", "ADA", "3,4,N", "ADA")
    val board = Board(7, 7)
    val l1 = Lawnmower(
      1,
      2,
      Orientation.E,
      Orientation.E,
      1,
      2,
      List(Action.D, Action.A, Action.G, Action.A)
    )
    val l2 = Lawnmower(
      3,
      4,
      Orientation.N,
      Orientation.N,
      3,
      4,
      List(Action.A, Action.D, Action.A)
    )
    val l3 = Lawnmower(
      3,
      4,
      Orientation.N,
      Orientation.N,
      3,
      4,
      List(Action.A, Action.D, Action.A)
    )
    val expectedLawnmowers: Either[WrongUserInput, List[Lawnmower]] =
      Right(List(l1, l2, l3))
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, goodParams, List())
    assert(result === expectedLawnmowers)
  }

  test(
    "parse lawnmowers should send error when lawnmower outside of the board"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("6,2,E", "DDD", "5,5,S", "ADG")
    val board = Board(5, 5)
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(
      result === Left(
        WrongUserInput(
          "Lawnmower with positions (6,2) can not be outside of the board (5,5)"
        )
      )
    )
  }

  test(
    "parse lawnmowers should send error when lawnmower pos is negative"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    val wrongParams = List("-6,2,E", "DDD", "5,5,3", "ADG")
    val board = Board(5, 5)
    val result: Either[WrongUserInput, List[Lawnmower]] =
      parser.parseLawnmowers(board, wrongParams, List())
    assert(
      result === Left(
        WrongUserInput(
          "Wrong position : -6"
        )
      )
    )
  }

  test(
    "parse string should return exception if board wrong"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")
    assert(
      parser.parseString() === Left(WrongUserInput("Wrong board parameter"))
    )
  }

  test(
    "parse string should return exception if lawnmower wrong"
  ) {
    val parser = new Parser("5, 5;1,2,E;DDD;1,3,2;GAGAGA;")
    assert(
      parser.parseString() === Left(WrongUserInput("Wrong orientation : 2"))
    )
  }

  test(
    "parse string should return good result"
  ) {
    val parser = new Parser("7, 7;1,2,E;DAGA;3,4,N;ADA;3,4,N;ADA;")
    val board = Board(7, 7)
    val l1 = Lawnmower(
      1,
      2,
      Orientation.E,
      Orientation.E,
      1,
      2,
      List(Action.D, Action.A, Action.G, Action.A)
    )
    val l2 = Lawnmower(
      3,
      4,
      Orientation.N,
      Orientation.N,
      3,
      4,
      List(Action.A, Action.D, Action.A)
    )
    val l3 = Lawnmower(
      3,
      4,
      Orientation.N,
      Orientation.N,
      3,
      4,
      List(Action.A, Action.D, Action.A)
    )

    val expectedLawnmowers = List(l1, l2, l3)
    val result = Result(board, expectedLawnmowers)
    assert(parser.parseString() === Right(result))
  }

}
