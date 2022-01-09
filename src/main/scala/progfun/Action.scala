package progfun

import exceptions.WrongUserInput

object Action extends Enumeration {
  type Action = Value

  val A, G, D = Value

  def mapToAction(action: Char): Either[WrongUserInput, Action] = action match {
    case 'A' => Right(A)
    case 'G' => Right(G)
    case 'D' => Right(D)
    //TODO mettre either ici aussi
    case _ => Left(WrongUserInput(s"(Wrong action : $action)"))
  }
}
