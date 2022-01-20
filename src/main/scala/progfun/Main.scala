package progfun

import com.typesafe.config.{Config, ConfigFactory}

import java.io.PrintWriter
import play.api.libs.json.Writes
import progfun.model.Result
import progfun.parser.Parser
import progfun.writer.WriteCsv

object Main extends App {
  val conf: Config = ConfigFactory.load()
  val input = conf.getString("application.input-file")
  val outputJSON = conf.getString("application.output-json-file")
  val outputCSV = conf.getString("application.output-csv-file")

  val parser = new Parser(input)
  parser.parseString() match {
    case Right(initialResult: Result) =>
      initialResult.perform(List(), initialResult.lawnmowers) match {
        case Right(value: Result) => {
          val resultJSON = Writes.of[Result].writes(value)
          val pw = new PrintWriter(outputJSON)
          pw.write(resultJSON.toString())
          pw.close()

          val writeCsv = new PrintWriter(outputCSV)
          writeCsv.write(WriteCsv.writesResult(value))
          writeCsv.close()
        }
        case Left(error: Error) => print(error)
      }
    case Left(error: Error) => println(error)
  }
}
