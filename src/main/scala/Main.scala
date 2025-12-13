object Main {
  def main(args: Array[String]): Unit = {

    // List of analysis tasks to run
    val tasks: List[AnalysisTask] = List(
      HighestBookingCountry,
      HotelAnalysis,
      MostProfitable
    )

    // Loop through each task and execute it
    for (task <- tasks) {
      println(s"Running ${task.getClass.getSimpleName}")
      task.run()
      println("-" * 40)
    }
  }
}
