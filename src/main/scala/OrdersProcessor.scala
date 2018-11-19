import scala.annotation.tailrec

/**
  * Created by Piotr on 19.11.2018.
  */


case class CustomerOrder(timeToOrder: Int, timeToCook: Int, waitingTime: Long = 0L)

object OrdersProcessor {

    def getAvgWaitingTime(customerOrders: List[CustomerOrder]):Int = {
      val sortedList = customerOrders.sortBy(c => c.timeToOrder)

      @tailrec
      def rec(lastProcessedCustomerOrderOpt: Option[CustomerOrder], co: List[CustomerOrder], waitingTimes: List[Long]): List[Long] = {
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
          case (None, customerOrder :: tail) =>
            val waitingTime = customerOrder.timeToOrder + customerOrder.timeToCook
            rec(Some(CustomerOrder(customerOrder.timeToOrder, customerOrder.timeToCook, waitingTime)), tail, waitingTime :: waitingTimes)
        }
      }
      val waitingTimes = rec(None, sortedList, List.empty[Long])
      waitingTimes match {
        case Nil => 0
        case _ => (sumAsLong(waitingTimes) / sortedList.size.toLong).toInt
      }
    }

  private def sumAsLong(list: List[Long]): Long = list.foldLeft(0L)((acc, e) => acc + e)
}
