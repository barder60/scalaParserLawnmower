package example.writer

import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.Writes
import progfun.model._
import progfun.writer.WriteCsv

class WriterSpec extends AnyFunSuite {

  test(
    "writer should write json with result object"
  ) {
    val board = Board(7, 7)
    val l1 = Lawnmower(
      1,
      1,
      2,
      Orientation.E,
      Orientation.E,
      2,
      1,
      List(Action.D, Action.A, Action.G, Action.A)
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
    val result = Result(board, lawnmowers)
    val resultJSON = Writes
      .of[Result]
      .writes(result)
    assert(
      resultJSON.toString()
        ===
        "{\"limite\":{\"x\":7,\"y\":7},\"tondeuses\":[{\"debut\":{\"point\":{\"x\":1,\"y\":2},\"direction\":\"E\"},\"instructions\":[\"D\",\"A\",\"G\",\"A\"],\"fin\":{\"point\":{\"x\":2,\"y\":1},\"direction\":\"E\"}},{\"debut\":{\"point\":{\"x\":3,\"y\":4},\"direction\":\"N\"},\"instructions\":[\"A\",\"D\",\"A\"],\"fin\":{\"point\":{\"x\":4,\"y\":5},\"direction\":\"E\"}},{\"debut\":{\"point\":{\"x\":3,\"y\":4},\"direction\":\"N\"},\"instructions\":[\"A\",\"D\",\"A\"],\"fin\":{\"point\":{\"x\":4,\"y\":5},\"direction\":\"E\"}}]}"
    )

  }

  test(
    "writer should write csv with result object"
  ) {
    val board = Board(7, 7)
    val l1 = Lawnmower(
      1,
      1,
      2,
      Orientation.E,
      Orientation.E,
      2,
      1,
      List(Action.D, Action.A, Action.G, Action.A)
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
    val result = Result(board, lawnmowers)
    val resultCsv = WriteCsv.writesResult(result)
    assert(
      resultCsv
        ===
        "numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;instructions\n1;1;2;E;2;1;E;AGAD\n2;3;4;N;4;5;E;ADA\n3;3;4;N;4;5;E;ADA\n\n"
    )

  }

}
