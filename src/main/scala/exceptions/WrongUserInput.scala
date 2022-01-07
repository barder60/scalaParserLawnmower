package exceptions

final case class WrongUserInput(message: String) extends Error {
  override def getMessage: String = message
}
