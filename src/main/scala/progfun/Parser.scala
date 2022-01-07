package progfun

import exceptions.WrongUserInput

class Parser(string: String) {
  //doit renvoyer les tondeuses azvec leurs actions
  //le plateau
  //totues les tondeuses
  //

  //parser qui prend en entrée une string "5,5;1,2,E;DDD;1,3,S;GAGAGA;"
  //qu'est-ce qu'il créé ? une premiere tondeuse
  //boucle avec toutes les tondeuses possibles
  //ensuite il appelle quelque chose pour que le traitement se lance j'imagine
  //mais dans quoi ? Un autre objet ?
  //bien faire gaf aux limites dnas la logique
  //une fosi que les traitements sont faits, on peut les récupérer via les
  //instances de nos tondeuses je pense


  //méthodes qu'on veut ici
  //chaque tondeuse avec leur actions
  def parseString() = {
    val elements = this.string.split(';')
    println(elements.mkString("Array(", ", ", ")"))
  }


  def parseBoard(boardElements: String): Either[WrongUserInput, Board] = {
    val elements: Array[String] = boardElements.split(',')
    if (elements.length != 2
      || !(elements(0) forall Character.isDigit)
      || !(elements(1) forall Character.isDigit)) {
      Left(WrongUserInput("Wrong board parameter"))
    } else {
      val limitX = elements(0).toInt
      val limitY = elements(1).toInt
      Right(Board(limitX, limitY))
    }

  }
}
