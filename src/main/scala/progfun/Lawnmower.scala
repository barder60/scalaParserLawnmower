package progfun

import progfun.Action.{A, Action, D, G}
import progfun.Direction.{left, right, Direction, E, N, S, W}

class Lawnmower(
    startX: Int,
    startY: Int,
    startOrientation: Direction,
    finalOrientation: Direction,
    finalX: Int,
    finalY: Int,
    actions: List[Action]
) {
  def turnRight(): Lawnmower = {
    new Lawnmower(
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
    new Lawnmower(
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
      new Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX,
        finalY + 1,
        actions
      )
    case S =>
      new Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX,
        finalY - 1,
        actions
      )
    case E =>
      new Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX + 1,
        finalY,
        actions
      )
    case W =>
      new Lawnmower(
        this.startX,
        this.startY,
        this.startOrientation,
        this.finalOrientation,
        finalX - 1,
        finalY,
        actions
      )
    case _ =>
      new Lawnmower(
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

  def AfficheX(): Int = {
    this.startX
  }

  def AfficheY(): Int = {
    this.startY
  }

  def AfficheOrientation(): Direction = {
    this.finalOrientation
  }
}
