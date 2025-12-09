import scala.io.Source

object HotelAnalysis extends App {

  // open CSV file from resources folder
  val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv") // format for csv have changed
  if (inputStream == null) throw new Exception("CSV file not found!") // stop if missing
  val source = Source.fromInputStream(inputStream, "ISO-8859-1")          // read as UTF-8 to prevent error when reading

  // read all lines and skip the header row
  val lines = source.getLines().drop(1).toList
  source.close()  // close file after reading

  // Data structure: (country, city, hotel, price, discount, profit)
  case class Booking(country: String, city: String, hotel: String,
                     price: Double, discount: Double, profit: Double)

  // parse CSV lines into a list of bookings
  val bookings = lines.flatMap { line =>
    val cols = line.split(",")

    // Only process rows with enough columns and valid numeric values
    if (cols.length >= 24 &&
      cols(20).forall(c => c.isDigit || c == '.') &&
      cols(21).forall(c => c.isDigit || c == '%') &&
      cols(23).forall(c => c.isDigit || c == '.')) {

      Some(Booking(
        country = cols(6).trim,
        city = cols(10).trim,
        hotel = cols(16).trim,
        price = cols(20).toDouble,
        discount = cols(21).replace("%", "").toDouble / 100,
        profit = cols(23).toDouble
      ))
    } else None
  }
  // Group by Country + City + Hotel
  val grouped = bookings.groupBy(b => (b.country, b.city, b.hotel))

  // Compute averages
  val averages = grouped.map { case (key, list) =>
    val avgPrice = list.map(_.price).sum / list.size
    val avgDiscount = list.map(_.discount).sum / list.size
    val avgProfit = list.map(_.profit).sum / list.size

    (key, avgPrice, avgDiscount, avgProfit)
  }.toList

  // Extract min/max for normalization
  val prices = averages.map(_._2)
  val discounts = averages.map(_._3)
  val profits = averages.map(_._4)

  //print the result
  println("Most Economical Hotel:")
  println("Destination     : " + cheapestHotel._2 + ", " + cheapestHotel._1) // UPDATED: Print City & Country
  println("Hotel Name      : " + cheapestHotel._3)
  println(f"Booking Price   : ${cheapestHotel._4}%.2f SGD")
  println(f"Discount        : ${cheapestHotel._5 * 100}%.0f%%")
  println(f"Total Cost      : $minCost%.2f SGD")
  println(f"Profit Margin   : ${cheapestHotel._6}%.2f")
}