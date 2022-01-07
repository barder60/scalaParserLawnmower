package progfun

import progfun.Action.{A, Action, D, G}
import progfun.Direction.{Direction, N, E, S, W, left, right}

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

class Lawnmower(x: Int, y: Int, orientation: Direction) {
  def turnRight(): Lawnmower = {
    new Lawnmower(this.x, this.y, right(this.orientation))
  }

  def turnLeft(): Lawnmower = {
    new Lawnmower(this.x, this.y, left(this.orientation))
  }

  def moveForward(): Lawnmower = this.orientation match {
    case N => new Lawnmower(this.x, this.y + 1, this.orientation)
    case S => new Lawnmower(this.x, this.y - 1, this.orientation)
    case E => new Lawnmower(this.x + 1, this.y, this.orientation)
    case W => new Lawnmower(this.x - 1, this.y, this.orientation)
    case _ => new Lawnmower(this.x, this.y, this.orientation)
  }

  def move(action: Action): Lawnmower = action match {
    case A => moveForward()
    case G => turnLeft()
    case D => turnRight()
  }


  def AfficheX(): Int = {
    this.x
  }

  def AfficheY(): Int = {
    this.y
  }

  def AfficheOrientation(): Direction = {
    this.orientation
  }
}


