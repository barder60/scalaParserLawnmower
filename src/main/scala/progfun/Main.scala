package progfun

object Main extends App {
  println("Ici le programme principal")
  val input = "5,5;1,2;;1,3;GAGAGA;"

  val limitX = 5
  val limitY = 5
  val indexDebut = 1
  val indexFin = 2
  val lawnmower1 = new Lawnmower(indexDebut, indexFin, Direction.N)

  println(lawnmower1.move(Action.G).AfficheX())
  println(lawnmower1.move(Action.G).AfficheY())
  println(lawnmower1.move(Action.G).AfficheOrientation())
}
