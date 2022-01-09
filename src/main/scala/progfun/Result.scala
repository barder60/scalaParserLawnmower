package progfun

import play.api.libs.json.{JsObject, Writes}

case class Result(board: Board, lawnmowers: List[Lawnmower])

object Result {
  implicit val writesResult: Writes[Result] = (Result: Result) => JsObject(
    Map(
      "limite" -> Writes.of[Board].writes(Result.board),
      "toundeuses" -> Writes.of[List[Lawnmower]].writes(Result.lawnmowers)
    )
  )
}
