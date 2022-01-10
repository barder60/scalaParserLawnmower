package progfun

import play.api.libs.json.{JsObject, Writes}
import exceptions.DonneesIncorectesException
import progfun.Action.Action

case class Result(board: Board, lawnmowers: List[Lawnmower]) {
  def perform(
               newLawnmowers: List[Lawnmower],
               lawnmowers: List[Lawnmower]
             ): Either[DonneesIncorectesException, Result] =
    lawnmowers match {
      case first :: rest =>
        moveLawnmower(first, first.actions) match {
          case Left(error: Error) => Left(error)
          case Right(newLawnmower: Lawnmower) =>
            perform(
              newLawnmower :: newLawnmowers,
              rest
            )
        }

      case Nil => Right(Result(this.board, newLawnmowers.reverse))
    }

  def moveLawnmower(
                     lawnmower: Lawnmower,
                     actions: List[Action]
                   ): Either[DonneesIncorectesException, Lawnmower] = {
    if (lawnmower.finalX > this.board.limitX || lawnmower.finalY > this.board.limitY) {
      Left(
        DonneesIncorectesException("Lawnmower hit board limit ".concat(lawnmower.toString))
      )
    } else {
      actions match {
        case first :: rest =>
          moveLawnmower(lawnmower.move(first), rest)
        case Nil => Right(lawnmower)
      }
    }
  }
}

object Result {
  implicit val writesResult: Writes[Result] = (Result: Result) =>
    JsObject(
      Map(
        "limite" -> Writes.of[Board].writes(Result.board),
        "tondeuses" -> Writes.of[List[Lawnmower]].writes(Result.lawnmowers)
      )
    )
}
