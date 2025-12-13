import scala.io.Source
import scala.util.Try

object MostProfitable extends AnalysisTask {
  override def run(): Unit = {

    // Case class to store hotel info relevant for profit calculation
    case class Booking(hotel: String, rooms: Int, profitMargin: Double)

    // Helper functions to safely convert strings to numbers
    def toIntSafe(s: String): Int = Try(s.trim.toInt).getOrElse(0)

    def toDoubleSafe(s: String): Double = Try(s.trim.toDouble).getOrElse(0.0)

    // Load CSV file
    val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv")
    if (inputStream == null) {
      println("CSV file not found!"); return
    }

    val lines = Source.fromInputStream(inputStream, "ISO-8859-1").getLines().toList
    inputStream.close() // close file stream

    // Parse CSV lines into Booking objects
    val data: List[Booking] = lines.tail.flatMap { line =>
      val cols = line.split(",").map(_.trim)
      if (cols.length >= 24) {
        Some(Booking(
          hotel = cols(16), // hotel name
          rooms = toIntSafe(cols(15)), // number of rooms booked
          profitMargin = toDoubleSafe(cols(23)) // profit margin per booking
        ))
      } else None // skip invalid rows
    }

    if (data.isEmpty) {
      println("No data available"); return
    }

    // Compute total profit per hotel (rooms × profit margin)
    val hotelProfits: Map[String, Double] = data
      .groupBy(_.hotel)
      .view
      .mapValues(bookings => bookings.map(b => b.rooms * b.profitMargin).sum)
      .toMap

    // Find hotel with highest total profit
    val (hotel, profit) = hotelProfits.maxBy(_._2)

    // Print results
    println("===== Q3: Most Profitable Hotel =====")
    println(s"Hotel: $hotel")
    println(f"Total Profit (rooms × margin): $profit%.2f")
    println("=" * 40)
  }
}
