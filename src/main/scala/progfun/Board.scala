package progfun

import play.api.libs.json.{JsObject, Writes}


case class Board(limitX: Int, limitY: Int)

object Board {
  def apply(limitX: Int, limitY: Int): Board = new Board(limitX, limitY)

  implicit val writesBoard: Writes[Board] = (v: Board) => JsObject(
    Map(
      "x" -> Writes.of[Int].writes(v.limitX),
      "y" -> Writes.of[Int].writes(v.limitY)
    )
  )
}


