package progfun

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

  def map(c: String): Direction = c match {
    case "E" => E
    case "S" => S
    case "W" => W
    case "N" => N
    //TODO renvoyer une excpetion avec Either ici ?
    case _ => N
  }
}
