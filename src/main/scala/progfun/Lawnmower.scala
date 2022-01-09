package progfun

import progfun.Action.{A, Action, D, G}
import progfun.Direction.{left, right, Direction, E, N, S, W}

case class Lawnmower(
    startX: Int,
    startY: Int,
    startOrientation: Direction,
    finalOrientation: Direction,
    finalX: Int,
    finalY: Int,
    actions: List[Action]
) {
  def turnRight(): Lawnmower = {
    Lawnmower(
      this.startX,
      this.startY,
      this.startOrientation,
      right(this.finalOrientation),
      finalX,
      finalY,
      actions
    )
  }

  def turnLeft(): Lawnmower = {
    Lawnmower(
      this.startX,
      this.startY,
      this.startOrientation,
      left(this.finalOrientation),
      finalX,
      finalY,
      actions
    )
  }

  def moveForward(): Lawnmower = this.finalOrientation match {
    case N =>
      Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX,
        finalY + 1,
        actions
      )
    case S =>
      Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX,
        finalY - 1,
        actions
      )
    case E =>
      Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX + 1,
        finalY,
        actions
      )
    case W =>
      Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX - 1,
        finalY,
        actions
      )
    case _ =>
      Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX,
        finalY,
        actions
      )
  }

  def move(action: Action): Lawnmower = action match {
    case A => moveForward()
    case G => turnLeft()
    case D => turnRight()
  }
}
