object HighestBookingCountry extends AnalysisTask {
  override def run(): Unit = {
    // Load the CSV file from resources
    val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv")

    // Read all lines, skip the header, convert to List
    val lines: List[String] = scala.io.Source.fromInputStream(inputStream, "ISO-8859-1")
      .getLines()
      .drop(1)  // skip header row
      .toList
    inputStream.close()  // close the file stream

    // Case class to hold each booking record
    case class Record(
                       id: String, date: String, time: String, customer: String, gender: String,
                       age: String, originCountry: String, state: String, location: String,
                       destinationCountry: String, destinationCity: String, noOfPeople: Int,
                       checkinDate: String, noOfDays: String, checkoutDate: String, rooms: String,
                       hotelName: String, hotelRating: String, paymentMode: String, bankName: String,
                       bookingPrice: Double, discount: Double, gst: String, profitMargin: Double
                     )

    // Parse CSV lines into Record objects, filter out invalid rows
    val records: List[Record] = lines.flatMap { row =>
      val cols = row.split(",").toList
      if (cols.length >= 24 && cols(11).forall(c => c.isDigit)) {  // ensure correct length & numeric noOfPeople
        Some(Record(
          cols(0), cols(1), cols(2), cols(3), cols(4), cols(5), cols(6), cols(7),
          cols(8), cols(9), cols(10), cols(11).toInt,
          cols(12), cols(13), cols(14), cols(15),
          cols(16), cols(17), cols(18), cols(19),
          cols(20).toDouble,  // booking price
          cols(21).replace("%","").toDouble / 100,  // discount as fraction
          cols(22),
          cols(23).toDouble  // profit margin
        ))
      } else None  // skip invalid rows
    }

    // Compute the country with the highest number of bookings
    // Use foldLeft to build a map of country -> count
    val (topCountry, count) = records.foldLeft(Map.empty[String, Int]) { (acc, r) =>
      acc.updated(r.destinationCountry, acc.getOrElse(r.destinationCountry, 0) + 1)
    }.maxBy(_._2)  // find the country with max bookings

    // Print results
    println("===== Q1: Highest Booking Country =====")
    println(s"Country: $topCountry")
    println(s"Number of bookings: $count")
    println("=" * 40)
  }
}
