import scala.io.Source

object BestHotelFinder extends App {

  // Load CSV file from resources
  val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv") // open CSV file, file format has been changed too
  if (inputStream == null) throw new Exception("CSV file not found!")    // error checking
  val source = Source.fromInputStream(inputStream, "UTF-8")            // read file as UTF-8 to prevent further error

  // Parse CSV into a List of bookings
  val bookings = source.getLines().drop(1).flatMap { line =>            // skip header row
    val cols = line.split(",")                                          // split line into columns
    if (cols.length >= 24) {                                            // making sure that  row has enough columns
      try {
        Some((
          cols(6).trim,                        // origin country
          cols(16).trim,                       // hotel name
          cols(20).toDouble,                   // booking price
          cols(21).replace("%","").toDouble/100, // discount and operation conversion( % to decimal)
          cols(23).toDouble,                   // profit margin
          cols(11).toInt                        // number of people
        ))
      } catch {
        case _: NumberFormatException => None  // skip rows with invalid numbers
      }
    } else None
  }.toList

  source.close() // Close the file after reading, a practice from the references

  // Find the most economical hotel
  val bestHotel = bookings.minBy { case (_, _, price, discount, profit, _) =>
    price * (1 - discount) / profit   // Calculate effective cost combining price, discount, and profit margin
  }

  // Print the result
  println(s"Most Economical Hotel: ${bestHotel._2}")         // hotel name
  println(f"Booking Price: ${bestHotel._3}%.2f SGD")        // price
  println(f"Discount     : ${bestHotel._4 * 100}%.0f%%")    // discount
  println(f"Profit Margin: ${bestHotel._5}%.2f")            // profit margin
}
