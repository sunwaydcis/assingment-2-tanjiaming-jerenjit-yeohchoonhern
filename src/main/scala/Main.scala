object Main {
  def main(args: Array[String]): Unit = {

    val tasks: List[AnalysisTask] = List(
      HighestBookingCountry,
      HotelAnalysis,
      MostProfitable
    )

    for (task <- tasks) {
      println(s"Running ${task.getClass.getSimpleName}")
      task.run()
      println("-" * 40)
    }
  }
}
