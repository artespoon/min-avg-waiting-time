import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

/**
  * Created by Piotr on 19.11.2018.
  */


case class CustomerOrder(timeToOrder: Int, timeToCook: Int, waitingTime: Int = 0)

object MinAvgWaitingTime {

    private def processOrders(customerOrders: List[CustomerOrder]):Int = {
    val sortedList = customerOrders.sortBy(c => c.timeToOrder)

    @tailrec
    def rec(lastProcessedCustomerOrderOpt: Option[CustomerOrder], co: List[CustomerOrder], waitingTimes: List[Int]): List[Int] = {
      (lastProcessedCustomerOrderOpt, co) match {
        case (_, Nil) => waitingTimes
        case (Some(lastProcessedCustomerOrder), customerOrder :: Nil) =>
          val customerOrderWaitingTime = lastProcessedCustomerOrder.timeToOrder + lastProcessedCustomerOrder.waitingTime - customerOrder.timeToOrder + customerOrder.timeToCook
          rec(None, Nil, customerOrderWaitingTime :: waitingTimes)
        case (Some(lastProcessedCustomerOrder), customerOrders @ head :: tail) =>
          val optimizedOrders = customerOrders.filter(_.timeToOrder <= lastProcessedCustomerOrder.waitingTime)
            .map(co => CustomerOrder(co.timeToOrder, co.timeToCook, lastProcessedCustomerOrder.timeToOrder + lastProcessedCustomerOrder.waitingTime - co.timeToOrder + co.timeToCook))
            .sortBy(_.waitingTime)
          rec(Some(optimizedOrders.head), optimizedOrders.tail, optimizedOrders.head.waitingTime :: waitingTimes)
        case (None, head :: tail) =>
          val waitingTime = head.timeToOrder + head.timeToCook
          rec(Some(CustomerOrder(head.timeToOrder, head.timeToCook, waitingTime)), tail, waitingTime :: waitingTimes)
      }
    }
    val waitingTimes = rec(None, sortedList, List.empty[Int])
    waitingTimes match {
      case Nil => 0
      case _ => (sumAsLong(waitingTimes) / sortedList.size.toLong).toInt
    }
  }

  private def sumAsLong(list: List[Int]): Long = list.foldLeft(0L)((acc, e) => acc + e)

  private def getNumberOfCustomerOrders(line: String): Int = {
    Try(line.toInt) match {
      case Success(s) => s
      case Failure(ex) => throw new NumberFormatException("Can't parse line: " + line + ". You should pass number of customers.")
    }
  }

  private def getCustomerOrder(line: String): CustomerOrder = {
    val parsedCustomerOrderLine = line.split(" ").toList.filter(!_.isEmpty)
    parsedCustomerOrderLine match {
      case timeToOrder :: timeToCook :: Nil => CustomerOrder(timeToOrder.toInt, timeToCook.toInt)
      case _ => throw new IllegalArgumentException("Can't parse line: [" + line + "]. You should pass in format of: [timeToOrder timeToCook] e.g. 1 6")
    }
  }

  def main(args: Array[String]) {
    val numberOfCustomers = getNumberOfCustomerOrders(StdIn.readLine())
    val customerOrders = (1 to numberOfCustomers).map( _ => getCustomerOrder(StdIn.readLine())).toList
    println(processOrders(customerOrders))

//     test()
  }

//  private def test(): Unit = {
//    println(getNumberOfCustomerOrders("1")) //1
//    println(getCustomerOrder("1 6")) //List(CustomerOrder(1,6,0)
//
//    println(processOrders(List()))  //0
//    println(processOrders(List(CustomerOrder(0,3), CustomerOrder(0,3), CustomerOrder(0,3)))) // must equals 6
//    println(processOrders(List(CustomerOrder(0,3), CustomerOrder(1,9), CustomerOrder(2,6)))) // must equals 9
//    println(processOrders(List(CustomerOrder(1,9), CustomerOrder(2,6), CustomerOrder(0,3)))) // must equals 9
//    println(processOrders(List(CustomerOrder(0,3), CustomerOrder(1,9), CustomerOrder(2,5)))) // must equals 8
//  }
}
