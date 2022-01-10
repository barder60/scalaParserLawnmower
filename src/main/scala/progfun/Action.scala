package progfun

import exceptions.DonneesIncorectesException

object Action extends Enumeration {
  type Action = Value

  val A, G, D = Value

  def mapToAction(action: Char): Either[DonneesIncorectesException, Action] = action match {
    case 'A' => Right(A)
    case 'G' => Right(G)
    case 'D' => Right(D)
    case _ => Left(DonneesIncorectesException("Wrong action : ".concat(action.toString)))
  }
}
