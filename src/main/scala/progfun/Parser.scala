package progfun

import exceptions.WrongUserInput
import progfun.Action.Action

class Parser(string: String) {

  //méthodes qu'on veut ici
  //chaque tondeuse avec leur actions
  //TODO après avoir reçu le résult, on peut boucler sur les tondeuses pour recréer l'objet Reesult avec les pos finales
  def parseString(): Either[WrongUserInput, Result] = {
    val elements: List[String] = this.string.split(';').map(_.trim).toList

    for {
      board      <- parseBoard(elements(0))
      lawnmowers <- parseLawnmowers(elements.drop(1), List())
    } yield new Result(board, lawnmowers)
  }

  def parseLawnmowers(
      lawnmowerElements: List[String],
      lawnmowers: List[Lawnmower]
  ): Either[WrongUserInput, List[Lawnmower]] = {
    if (lawnmowerElements.isEmpty) {
      Right(lawnmowers.reverse)
    } else if (lawnmowerElements.length % 2 != 0) {
      Left(WrongUserInput("Wrong lawnmowers parameter number"))
    } else {
      createLawnmower(lawnmowerElements(0), lawnmowerElements(1)) match {
        case Right(newLawnmower: Lawnmower) =>
          lawnmowers match {
            case x :: rest =>
              parseLawnmowers(
                lawnmowerElements.drop(2),
                newLawnmower :: x :: rest
              )
            case Nil =>
              parseLawnmowers(
                lawnmowerElements.drop(2),
                List(newLawnmower)
              )
          }
        case Left(error: Error) => Left(error)
      }
    }
  }

  def createLawnmower(
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
          startOrientation <- Direction.map(initPositions(2))
          actions          <- createActions(actionsString, List())
        } yield Lawnmower(
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
          actions match { //ah merded onc en gros
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
