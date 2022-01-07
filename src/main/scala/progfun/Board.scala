package progfun

//import progfun.Action.Action

class Board(limitX: Int, limitY: Int) {

  def limitX(): Int = limitX

  def limitY(): Int = limitY



  //  def run(lawnmower: Lawnmower, actions: Array[Action]): Lawnmower = {
  //    //boucle pour bouger la tondeuse
  //    new Lawnmower(this.limitX, (this.limitY), Direction.N)
  //  }
}

//
object Board {
  def apply(limitX: Int, limitY: Int): Board = new Board(limitX, limitY)
}