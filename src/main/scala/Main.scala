import scala.io.StdIn
import scala.util.{Failure, Success, Try}

/**
  * Created by Piotr on 19.11.2018.
  */
object Main {

  def getNumberOfCustomerOrders(line: String): Int = {
    Try(line.trim.toInt) match {
      case Success(s) => s
      case Failure(ex) => throw new NumberFormatException("Can't parse line: " + line + ". You should pass number of customers.")
    }
  }

  def getCustomerOrder(line: String): CustomerOrder = {
    val parsedCustomerOrderLine = line.split(" ").toList.filter(!_.isEmpty)
    parsedCustomerOrderLine match {
      case timeToOrder :: timeToCook :: Nil => CustomerOrder(timeToOrder.toInt, timeToCook.toInt)
      case _ => throw new IllegalArgumentException("Can't parse line: [" + line + "]. You should pass in format of: [timeToOrder timeToCook] e.g. 1 6")
    }
  }

  def main(args: Array[String]) {
    val numberOfCustomers = getNumberOfCustomerOrders(StdIn.readLine())
    val customerOrders = (1 to numberOfCustomers).map( _ => getCustomerOrder(StdIn.readLine())).toList
    println(OrdersProcessor.getAvgWaitingTime(customerOrders))
  }
}
