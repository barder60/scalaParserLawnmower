package exceptions

final case class DonneesIncorectesException(message: String) extends Error {
  override def getMessage: String = message
}
