package progfun

import progfun.Action.Action
import progfun.Orientation.Orientation

trait WritesCSV[A] {
  def apply(t: A): String
}

case object WriteCsv {
  implicit val writesInt: WritesCSV[Int] = (v: Int) => v.toString

  implicit val writesString: WritesCSV[String] = (v: String) => v

  implicit val writesOrientation: WritesCSV[Orientation] = (v: Orientation) => v.toString

  implicit val writesAction: WritesCSV[Action] = (v: Action) => v.toString

  implicit val writesActions: WritesCSV[List[Action]] = {
    case currentAction :: otherActions =>
      writesActions(otherActions) + writesAction(currentAction)
    case Nil => ""
  }

  implicit val writesLawnmower: WritesCSV[Lawnmower] = (v: Lawnmower) =>
    s"${writesInt(v.id)};${writesInt(v.startX)};${writesInt(v.startY)};${writesOrientation(v.startOrientation)};${writesInt(v.finalX)};${writesInt(v.finalY)};${writesOrientation(v.finalOrientation)};${writesActions(v.actions)}\n"

  implicit val writesLawnmowers: WritesCSV[List[Lawnmower]] = {
    case currentLawnmower :: othersLawnmowers =>
      writesLawnmower(currentLawnmower) + writesLawnmowers(othersLawnmowers)
    case Nil => "\n"
  }

  implicit val writesResult: WritesCSV[Result] = (v: Result) => s"numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;instructions" +
    s"\n\n${writesLawnmowers(v.lawnmowers)}"
}






