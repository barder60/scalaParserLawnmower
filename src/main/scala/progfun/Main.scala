package progfun

import play.api.libs.json.Writes

object Main extends App {

  val input = "7, 7;1,2,E;DAGA;3,4,N;ADA;3,4,N;ADA;"
  val parser = new Parser(input)
  parser.parseString() match {
    case Right(initialResult: Result) =>
      initialResult.perform(List(), initialResult.lawnmowers) match {
        case Right(value: Result) => println(Writes.of[Result].writes(value))
        case Left(error: Error) => print(error)
      }
    case Left(error: Error) => println(error)
  }
}
