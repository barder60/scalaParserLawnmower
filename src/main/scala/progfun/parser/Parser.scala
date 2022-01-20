package progfun.parser

import exceptions.WrongUserInput
import progfun.model.Action.Action
import progfun.model.{Action, Board, Lawnmower, Orientation, Result}

class Parser(string: String) {

  def parseString(): Either[WrongUserInput, Result] = {
    val elements: List[String] = this.string.split(';').map(_.trim).toList

    for {
      board <- parseBoard(elements(0))
      lawnmowers <- parseLawnmowers(board, elements.drop(1), List())
    } yield Result(board, lawnmowers)
  }

  def parseLawnmowers(
      board: Board,
      lawnmowerElements: List[String],
      lawnmowers: List[Lawnmower]
  ): Either[WrongUserInput, List[Lawnmower]] = {
    if (lawnmowerElements.isEmpty) {
      Right(lawnmowers.reverse)
    } else if (lawnmowerElements.length % 2 != 0) {
      Left(WrongUserInput("Wrong lawnmowers parameter number"))
    } else {
      createLawnmower(
        lawnmowers.length + 1,
        lawnmowerElements(0),
        lawnmowerElements(1)
      ) match {
        case Right(newLawnmower: Lawnmower) =>
          if (newLawnmower.startX > board.limitX || newLawnmower.startY > board.limitY) {
            Left(
              WrongUserInput(
                "Lawnmower with positions ("
                  .concat(newLawnmower.startX.toString)
                  .concat(",")
                  .concat(newLawnmower.startY.toString)
                  .concat(") can not be outside of the board (")
                  .concat(board.limitX.toString)
                  .concat(",")
                  .concat(board.limitY.toString)
                  .concat(")")
              )
            )
          } else {
            lawnmowers match {
              case x :: rest =>
                parseLawnmowers(
                  board,
                  lawnmowerElements.drop(2),
                  newLawnmower :: x :: rest
                )
              case Nil =>
                parseLawnmowers(
                  board,
                  lawnmowerElements.drop(2),
                  List(newLawnmower)
                )
            }
          }

        case Left(error: Error) => Left(error)
      }
    }
  }

  def createLawnmower(
                       lawnmoerId: Int,
                       positions: String,
                       actionsString: String
                     ): Either[WrongUserInput, Lawnmower] = {
    val initPositions = positions.split(',').map(_.trim).toList
    if (initPositions.length != 3) {
      Left(WrongUserInput("Wrong lawnmowers position / orientation"))
    } else {
      if (!(initPositions(0) forall Character.isDigit)) {
        Left(WrongUserInput("Wrong position : ".concat(initPositions(0))))
      } else if (!(initPositions(1) forall Character.isDigit)) {
        Left(WrongUserInput("Wrong position : ".concat(initPositions(1))))
      } else {
        val startX = initPositions(0).toInt
        val startY = initPositions(1).toInt
        for {
          startOrientation <- Orientation.map(initPositions(2))
          actions <- createActions(actionsString, List())
        } yield Lawnmower(
          lawnmoerId,
          startX,
          startY,
          startOrientation,
          startOrientation,
          startX,
          startY,
          actions
        )
      }

    }
  }

  def createActions(
      actionsString: String,
      actions: List[Action]
  ): Either[WrongUserInput, List[Action]] = {
    if (actionsString.nonEmpty) {
      Action.mapToAction(actionsString.head) match {
        case Right(newAction: Action) =>
          actions match {
            case x :: rest =>
              createActions(
                actionsString.substring(1),
                newAction :: x :: rest
              )
            case Nil =>
              createActions(
                actionsString.substring(1),
                List(newAction)
              )
          }
        case Left(error: Error) => Left(error)
      }

    } else {
      Right(actions.reverse)
    }

  }

  def parseBoard(boardElements: String): Either[WrongUserInput, Board] = {
    val elements: List[String] = boardElements.split(',').map(_.trim).toList
    if (elements.length != 2
        || !(elements(0) forall Character.isDigit)
        || !(elements(1) forall Character.isDigit)) {
      Left(WrongUserInput("Wrong board parameter"))
    } else {
      val limitX = elements(0).toInt
      val limitY = elements(1).toInt
      Right(Board(limitX, limitY))
    }

  }
}
