import scala.io.Source

object HotelAnalysis extends AnalysisTask {
  override def run(): Unit = {

    // Load CSV file from resources
    val inputStream = getClass.getResourceAsStream("/Hotel_Dataset.csv")
    if (inputStream == null) throw new Exception("CSV file not found!")
    val source = Source.fromInputStream(inputStream, "ISO-8859-1")
    val lines = source.getLines().drop(1).toList  // skip header row
    source.close()  // close the file stream

    // Case class to store relevant booking info
    case class Booking(country: String, city: String, hotel: String,
                       price: Double, discount: Double, profit: Double)

    // Parse CSV lines into Booking objects, filter out invalid rows
    val bookings = lines.flatMap { line =>
      val cols = line.split(",")
      if (cols.length >= 24 &&
        cols(20).forall(c => c.isDigit || c == '.') &&  // ensure price is numeric
        cols(21).forall(c => c.isDigit || c == '%') && // ensure discount format
        cols(23).forall(c => c.isDigit || c == '.')) { // ensure profit is numeric
        Some(Booking(
          country = cols(9).trim,
          city = cols(10).trim,
          hotel = cols(16).trim,
          price = cols(20).toDouble,
          discount = cols(21).replace("%","").toDouble / 100, // convert to fraction
          profit = cols(23).toDouble
        ))
      } else None
    }

    // Compute averages of price, discount, profit per hotel
    val averages = bookings
      .groupBy(b => (b.country, b.city, b.hotel))
      .view
      .map { case (key, list) =>
        val avgPrice = list.map(_.price).sum / list.size
        val avgDiscount = list.map(_.discount).sum / list.size
        val avgProfit = list.map(_.profit).sum / list.size
        (key, avgPrice, avgDiscount, avgProfit)
      }.toList

    // Find min and max for normalization in one fold
    val (minPrice, maxPrice, minDiscount, maxDiscount, minProfit, maxProfit) =
      averages.foldLeft((Double.MaxValue, Double.MinValue, Double.MaxValue, Double.MinValue, Double.MaxValue, Double.MinValue)) {
        case ((minP, maxP, minD, maxD, minPr, maxPr), (_, avgP, avgD, avgPr)) =>
          (math.min(minP, avgP), math.max(maxP, avgP),
            math.min(minD, avgD), math.max(maxD, avgD),
            math.min(minPr, avgPr), math.max(maxPr, avgPr))
      }

    // Normalize values to a score between 0-100 (higher better)
    def normalize(value: Double, min: Double, max: Double): Double =
      if (max - min == 0) 0 else (1 - ((value - min) / (max - min))) * 100

    // Compute final score for each hotel based on price, discount, profit
    val scored = averages.map { case (key, avgPrice, avgDiscount, avgProfit) =>
      val priceScore = normalize(avgPrice, minPrice, maxPrice)
      val discountScore = normalize(avgDiscount, minDiscount, maxDiscount)
      val profitScore = normalize(avgProfit, minProfit, maxProfit)
      val finalScore = (priceScore + discountScore + profitScore) / 3
      (key, finalScore, avgPrice, avgDiscount, avgProfit)
    }

    // Find the hotel with the highest final score
    val best = scored.maxBy(_._2)
    val ((country, city, hotel), score, avgP, avgD, avgPM) = best

    // Print results
    println("===== Q2: Most Economical Hotel =====")
    println(s"Hotel: $hotel")
    println(s"Destination: $city, $country")
    println(f"Average Price: $avgP%.2f SGD")
    println(f"Average Discount: ${avgD * 100}%.0f%%")
    println(f"Average Profit: $avgPM%.2f")
    println("=" * 40)
  }
}
