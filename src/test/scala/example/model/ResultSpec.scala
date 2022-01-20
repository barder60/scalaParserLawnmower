package example.model

import exceptions.WrongUserInput
import org.scalatest.funsuite.AnyFunSuite
import progfun.model._
import progfun.parser.Parser

class ResultSpec extends AnyFunSuite {

  test(
    "perform should return error if lawnmower hit board limit"
  ) {
    val input = "5,5;4,1,E;AAG;3,4,N;ADA;3,4,N;ADA;"
    val parser = new Parser(input)
    val initialResult = parser.parseString()
    for {
      result <- initialResult
    } yield assert(
      result.perform(List(), result.lawnmowers) === Left(
        WrongUserInput(
          "Lawnmower hit board limit Lawnmower(1,4,1,E,E,6,1,List(A, A, G))"
        )
      )
    )
  }

  test(
    "perform should return new result"
  ) {
    val board = Board(7, 7)
    val l1 = Lawnmower(
      1,
      4,
      1,
      Orientation.E,
      Orientation.N,
      6,
      1,
      List(Action.A, Action.A, Action.G)
    )
    val l2 = Lawnmower(
      2,
      3,
      4,
      Orientation.N,
      Orientation.E,
      4,
      5,
      List(Action.A, Action.D, Action.A)
    )
    val l3 = Lawnmower(
      3,
      3,
      4,
      Orientation.N,
      Orientation.E,
      4,
      5,
      List(Action.A, Action.D, Action.A)
    )
    val lawnmowers = List(l1, l2, l3)
    val expectedResult = Result(board, lawnmowers)
    val input = "7,7;4,1,E;AAG;3,4,N;ADA;3,4,N;ADA;"
    val parser = new Parser(input)
    val initialResult = parser.parseString()
    for {initialResult <- initialResult} yield assert(
      initialResult
        .perform(List(), initialResult.lawnmowers) === Right(expectedResult)
    )
  }

}
