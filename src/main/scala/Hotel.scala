import scala.io.Source

object HotelAnalysis extends App {
  // getResourceAsStream reads file from resources folder
  val fileStream = getClass.getResourceAsStream("/Hotel_Dataset.csv")
  val source = Source.fromInputStream(fileStream)

  for (line <- source.getLines()) {
    println(line)
  }

  source.close()
}