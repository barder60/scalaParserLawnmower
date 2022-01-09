package progfun

import play.api.libs.json.Writes
import progfun.Action.{A, D, G}

//Résumé du programme
//1. Prendre une string en entrée puis la parser
//2. Faire toute la logique de la tondeuse pour qu'elle se déplace
//3. Ecrire le résultat dans un fichier json puis dans un fichier csv
//Faire gaf aux typeclasses pour le csv

object Main extends App {
//  println("Ici le programme principal")
//  val input = "5,5;1,2;;1,3;GAGAGA;"
//
//  val parser = new Parser("5,5;1,2,E;DDD;1,3,S;GAGAGA;")
//  parser.parseString()


  val input = "7, 7;1,2,E;DAGA;3,4,N;ADA;3,4,N;ADA;"
  val parser = new Parser(input)
  val initialResult = parser.parseString()
  for {
    result <- initialResult
  } {
    val finalResult = result.perform(List(), result.lawnmowers)
    println(finalResult)
  }
//  val board = Board(5, 5) //juste pour test t'as capté
//  println(board.limitX())
//  println(board.limitY())

  val limitX = 5
  val limitY = 5
  val indexDebut = 1
  val indexFin = 2
  val board = new Board(limitX, limitY)
  val lawnmower1 = new Lawnmower(indexDebut, indexFin, Direction.N, Direction.N, limitX, limitY, List(G,A,D,G,A,G,A))
  val lawnmower2 = new Lawnmower(indexDebut, indexFin, Direction.S, Direction.S, limitX, limitY, List(G,A,D,G,A,G,A))

  val resultClass = new Result(board, List(lawnmower1, lawnmower2))

  val resultJSON = Writes.of[Result].writes(resultClass)
  println(resultJSON)
}
