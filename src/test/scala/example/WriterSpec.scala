package example

import exceptions.WrongUserInput
import org.scalatest.funsuite.AnyFunSuite
import progfun._

class WriterSpec extends AnyFunSuite {

  test(
    "writer should write json with result object"
  ) {
    val parser = new Parser("43;1,2,E;DDD;1,3,S;GAGAGA;")

    val result: Either[WrongUserInput, Board] = parser.parseBoard("43")
    assert(result === Left(WrongUserInput("Wrong board parameter")))
  }

}
