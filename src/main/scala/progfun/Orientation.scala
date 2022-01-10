package progfun

import exceptions.DonneesIncorectesException

object Orientation extends Enumeration {
  type Orientation = Value

  val N, E, W, S = Value

  def right(orientation: Value): Orientation = orientation match {
    case N => E
    case E => S
    case S => W
    case W => N
    case _ => orientation
  }

  def left(orientation: Value): Orientation = orientation match {
    case E => N
    case S => E
    case W => S
    case N => W
    case _ => orientation
  }

  def map(c: String): Either[DonneesIncorectesException, Orientation] = c match {
    case "E" => Right(E)
    case "S" => Right(S)
    case "W" => Right(W)
    case "N" => Right(N)
    case _ => Left(DonneesIncorectesException("Wrong orientation : ".concat(c)))
  }
}
