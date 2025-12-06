import scala.io.Source
import scala.util.{Try, Success, Failure}

object MostProfitable {

  case class Booking(hotel: String, rooms: Int, profitMargin: Double)

  def toIntSafe(s: String): Int = Try(s.trim.toInt).getOrElse(0)
  def toDoubleSafe(s: String): Double = Try(s.trim.toDouble).getOrElse(0.0)

  
  def main(args: Array[String]): Unit = {
  }
}



