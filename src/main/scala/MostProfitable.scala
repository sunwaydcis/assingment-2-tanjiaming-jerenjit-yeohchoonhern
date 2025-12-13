import scala.io.Source
import scala.util.{Try, Success, Failure}

object MostProfitable extends AnalysisTask {
  
  case class Booking(hotel: String, rooms: Int, profitMargin: Double)

  def toIntSafe(s: String): Int = Try(s.trim.toInt).getOrElse(0)
  def toDoubleSafe(s: String): Double = Try(s.trim.toDouble).getOrElse(0.0)

  def loadData(): List[Booking] = {
    val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv")
    if (inputStream == null) {
      println("CSV file not found!")
      return List.empty
    }

    val lines = Source.fromInputStream(inputStream, "ISO-8859-1").getLines().toList
    inputStream.close()

    if (lines.isEmpty) {
      println("CSV is empty!")
      return List.empty
    }

    lines.tail.flatMap { line =>
      val cols = line.split(",").map(_.trim)
      if (cols.length >= 24) {
        Some(Booking(
          hotel = cols(16),
          rooms = toIntSafe(cols(15)),
          profitMargin = toDoubleSafe(cols(23))
        ))
      } else None
    }
  }

  def findMostProfitable(data: List[Booking]): Option[(String, Double)] = {
    val totalProfits = data.groupBy(_.hotel).map { case (hotel, bookings) =>
      hotel -> bookings.map(b => b.rooms * b.profitMargin).sum
    }
    totalProfits.maxByOption(_._2)
  }

  override def run(): Unit = {
    val data = loadData()
    if (data.isEmpty) {
      println("No data available to compute profits.")
      return
    }

    findMostProfitable(data) match {
      case Some((hotel, profit)) =>
        println(s"Most Profitable Hotel: $hotel")
        println(f"Total Profit (rooms × margin): $profit%.2f")
      case None =>
        println("Unable to find the most profitable hotel.")
    }
  }
//
//  def main(args: Array[String]): Unit = {
//    run()
//  }
}

