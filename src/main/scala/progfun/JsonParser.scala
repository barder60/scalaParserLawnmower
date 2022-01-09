package progfun

import play.api.libs.json.{JsFalse, JsNumber, JsString, JsTrue, JsValue}
import progfun.Direction.Direction

case class JsonParser() {

  trait Writes[A] {
    def writes(v: A): JsValue
  }
  trait WritesTwo[A, B] {
    def writes(v: A, w: B): JsValue
  }

  object Writes {
    implicit val writesInt: Writes[Int] = (v: Int) => JsNumber(v)

    implicit val writesString: Writes[String] = (v: String) => JsString(v)

    implicit val writesBoolean: Writes[Boolean] = (v: Boolean) => {
      if (v) JsTrue
      else JsFalse
    }

    implicit val writesDirection: Writes[Direction] = (v: Direction) => JsString(v.toString)

    def of[A](implicit w: Writes[A]): Writes[A] = w
  }
}
