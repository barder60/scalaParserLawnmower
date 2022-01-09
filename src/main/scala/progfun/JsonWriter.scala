package progfun

import play.api.libs.json.{JsFalse, JsNumber, JsString, JsTrue, JsValue}
import progfun.Orientation.Orientation

case class JsonWriter() {

  trait Writes[A] {
    def writes(v: A): JsValue
  }

  object Writes {
    implicit val writesInt: Writes[Int] = (v: Int) => JsNumber(v)

    implicit val writesString: Writes[String] = (v: String) => JsString(v)

    implicit val writesBoolean: Writes[Boolean] = (v: Boolean) => {
      if (v) JsTrue
      else JsFalse
    }

    implicit val writesOrientation: Writes[Orientation] = (v: Orientation) =>
      JsString(v.toString)

    def of[A](implicit w: Writes[A]): Writes[A] = w
  }
}
