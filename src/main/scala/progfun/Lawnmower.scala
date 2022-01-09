package progfun

import play.api.libs.json.{JsObject, Writes}

import progfun.Action.{A, Action, D, G}
import progfun.Orientation.{left, right, Orientation, E, N, S, W}

case class Lawnmower(
                      startX: Int,
                      startY: Int,
                      startOrientation: Orientation,
                      finalOrientation: Orientation,
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

object Lawnmower {
  implicit val writesJSON: Writes[Lawnmower] = (v: Lawnmower) =>
    JsObject(
      Map(
        "debut" -> JsObject(
          Map(
            "point" -> JsObject(
              Map(
                "x" -> Writes.of[Int].writes(v.startX),
                "y" -> Writes.of[Int].writes(v.startY)
              )
            ),
            "direction" -> Writes.of[Orientation].writes(v.startOrientation)
          )
        ),
        "instructions" -> Writes.of[List[Action]].writes(v.actions),
        "fin" -> JsObject(
          Map(
            "point" -> JsObject(
              Map(
                "x" -> Writes.of[Int].writes(v.finalX),
                "y" -> Writes.of[Int].writes(v.finalY)
              )
            ),
            "direction" -> Writes.of[Orientation].writes(v.finalOrientation)
          )
        )
      )
    )
}
