package progfun

import exceptions.WrongUserInput

object Direction extends Enumeration {
  type Direction = Value

  val N, E, W, S = Value

  def right(direction: Value): Direction = direction match {
    case N => E
    case E => S
    case S => W
    case W => N
    case _ => direction
  }

  def left(direction: Value): Direction = direction match {
    case E => N
    case S => E
    case W => S
    case N => W
    case _ => direction
  }

  def map(c: String): Either[WrongUserInput, Direction] = c match {
    case "E" => Right(E)
    case "S" => Right(S)
    case "W" => Right(W)
    case "N" => Right(N)
    case _   => Left(WrongUserInput("Wrong orientation : ".concat(c)))
  }
}
