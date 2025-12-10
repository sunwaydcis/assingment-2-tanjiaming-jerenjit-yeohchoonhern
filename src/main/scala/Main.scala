object Main {
  def main(args: Array[String]): Unit = { // polymorphyism in q123, abstract run in q123, forloop for main
    println("Running HighestBookingCountry...")
    HighestBookingCountry.run()

    println("\nRunning HotelAnalysis...")
    HotelAnalysis.run()

    println("\nRunning MostProfitable...")
    MostProfitable.run()

  }
}