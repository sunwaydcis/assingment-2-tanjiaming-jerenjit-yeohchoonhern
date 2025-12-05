import java.io.File
import scala.io.Source

// Case class to describe a record
case class Record(
                   id: String, date: String, time: String, customer: String, gender: String,
                   age: String, originCountry: String, state: String, location: String,
                   destinationCountry: String, destinationCity: String, noOfPeople: Int,
                   checkinDate: String, noOfDays: String, checkoutDate: String, rooms: String,
                   hotelName: String, hotelRating: String, paymentMode: String, bankName: String,
                   bookingPrice: Double, discount: Double, gst: String, profitMargin: Double
                 )

object HighestBookingCountry {
  def main(args: Array[String]): Unit = {
//    val file = new File("Hotel_Dataset.csv") // just use the filename
    val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv")

    // read lines from CSV and skip the header
  //    val lines: List[String] = Source.fromFile(file, "ISO-8859-1").getLines().drop(1).toList
    val lines: List[String] = scala.io.Source.fromInputStream(inputStream, "ISO-8859-1").getLines().drop(1).toList
    inputStream.close()
    // Convert each line into a list of columns
    val rows: List[List[String]] = lines.map(line => line.split(",").toList)

    // Skip header and convert to Record objects
    val records: List[Record] = rows.flatMap { row =>
      if (row.length >= 24 && row(11).forall(c => c.isDigit)) {   // only process if column 11 is numeric
        Some(Record(
          row(0), row(1), row(2), row(3), row(4), row(5), row(6), row(7),
          row(8), row(9), row(10), row(11).toInt,
          row(12), row(13), row(14), row(15),
          row(16), row(17), row(18), row(19),
          row(20).toDouble,
          row(21).replace("%","").toDouble / 100,
          row(22),
          row(23).toDouble
        ))
      } else None
    }




    // Count bookings per origin country
    val countryCounts = records
      .groupBy(_.originCountry)
      .view.mapValues(_.size).toMap

    // Find country with max bookings
    val (topCountry, count) = countryCounts.maxBy(_._2)
    println(s"Country with the highest number of bookings: $topCountry ($count bookings)")

  }
}
