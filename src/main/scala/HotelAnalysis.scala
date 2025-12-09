import scala.io.Source

object HotelAnalysis extends App {

  // open CSV file from resources folder
  val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv") // format for csv have changed
  if (inputStream == null) throw new Exception("CSV file not found!") // stop if missing
  val source = Source.fromInputStream(inputStream, "ISO-8859-1")          // read as UTF-8 to prevent error when reading

  // read all lines and skip the header row
  val lines = source.getLines().drop(1).toList
  source.close()  // close file after reading

  // parse CSV lines into a list of bookings
  var bookings = List.empty[(String, String, String, Double, Double, Double, Int)] // initialize empty list

  for (line <- lines) {
    val cols = line.split(",")

    // Only process rows with enough columns and valid numeric values
    if (cols.length >= 24 &&
      cols(20).forall(c => c.isDigit || c == '.') &&
      cols(21).forall(c => c.isDigit || c == '%') &&
      cols(23).forall(c => c.isDigit || c == '.')) {

      val destinationCountry = cols(6).trim
      val destinationCity    = cols(10).trim // UPDATED: Get City from Index 10
      val hotelName     = cols(16).trim
      val price         = cols(20).toDouble
      val discount      = cols(21).replace("%","").toDouble / 100 // percent to decimal
      val profitMargin  = cols(23).toDouble
      val numPeople     = cols(11).toInt

      bookings = bookings :+ (destinationCountry, hotelName, price, discount, profitMargin, numPeople)
    }
  }

  // find the economical hotel using loop
  var cheapestHotel = bookings.head
  var minCost = cheapestHotel._3 * (1 - cheapestHotel._4) / cheapestHotel._5

  for (hotel <- bookings.tail) {
    val effectiveCost = hotel._3 * (1 - hotel._4) / hotel._5
    if (effectiveCost < minCost) {
      cheapestHotel = hotel
      minCost = effectiveCost
    }
  }

  //print the result
  println("Most Economical Hotel:")
  println("Hotel Name   : " + cheapestHotel._2)
  println(f"Booking Price: ${cheapestHotel._3}%.2f SGD")
  println(f"Discount     : ${cheapestHotel._4 * 100}%.0f%%")
  println(f"Profit Margin: ${cheapestHotel._5}%.2f")
}