package progfun

import play.api.libs.json.{JsObject, Writes}
import progfun.Action.{A, Action, D, G}
import progfun.Direction.{Direction, E, N, S, W, left, right}

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
}

case class Lawnmower(startX: Int, startY: Int, orientation: Direction, finalX: Int, finalY: Int, actions: List[Action]) {
  def turnRight(): Lawnmower = {
    new Lawnmower(this.startX, this.startY, right(this.orientation), finalX, finalY, actions)
  }

  def turnLeft(): Lawnmower = {
    new Lawnmower(this.startX, this.startY, left(this.orientation), finalX, finalY, actions)
  }

  def moveForward(): Lawnmower = this.orientation match {
    case N => new Lawnmower(this.startX, this.startY, this.orientation, finalX, finalY + 1, actions)
    case S => new Lawnmower(this.startX, this.startY, this.orientation, finalX, finalY - 1, actions)
    case E => new Lawnmower(this.startX, this.startY, this.orientation, finalX + 1, finalY, actions)
    case W => new Lawnmower(this.startX, this.startY, this.orientation, finalX - 1, finalY, actions)
    case _ => new Lawnmower(this.startX, this.startY, this.orientation, finalX, finalY, actions)
  }

  def move(action: Action): Lawnmower = action match {
    case A => moveForward()
    case G => turnLeft()
    case D => turnRight()
  }


  def AfficheX(): Int = {
    this.startX
  }

  def AfficheY(): Int = {
    this.startY
  }

  def AfficheOrientation(): Direction = {
    this.orientation
  }
}

object Lawnmower {
  implicit val writesJSON: Writes[Lawnmower] = (v: Lawnmower) => JsObject(
    Map(
      "debut" -> JsObject(Map(
        "point" -> JsObject(Map(
          "x" -> Writes.of[Int].writes(v.startX),
          "y" -> Writes.of[Int].writes(v.startY),
        )),
        "direction" -> Writes.of[Direction].writes(v.orientation)
      )
    ), "fin" -> JsObject(Map(
        "point" -> JsObject(Map(
          "x" -> Writes.of[Int].writes(v.finalX),
          "y" -> Writes.of[Int].writes(v.finalY),
        )),
        "direction" -> Writes.of[Direction].writes(v.orientation)
      ))
  ))
}

