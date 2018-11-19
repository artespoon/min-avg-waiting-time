import scala.annotation.tailrec

/**
  * Created by Piotr on 19.11.2018.
  */


case class CustomerOrder(timeToOrder: Int, timeToCook: Int, waitingTime: Long = 0L)

object OrdersProcessor {

    def getAvgWaitingTime(customerOrders: List[CustomerOrder]):Long = {
      val sortedList = customerOrders.sortBy(_.timeToOrder)

      @tailrec
      def rec(lastProcessedCustomerOrderOpt: Option[CustomerOrder], co: List[CustomerOrder], waitingTimes: List[Long]): List[Long] = {
        (lastProcessedCustomerOrderOpt, co) match {
          case (_, Nil) => waitingTimes
          case (Some(lastProcessedCustomerOrder), customerOrder :: Nil) =>
            val customerOrderWaitingTime = computeWaitingTime(Some(lastProcessedCustomerOrder), customerOrder)
            rec(None, Nil, customerOrderWaitingTime :: waitingTimes)
          case (Some(lastProcessedCustomerOrder), customerOrders @ head :: tail) =>
            val overlappingCustomerOrders = customerOrders.filter(_.timeToOrder <= lastProcessedCustomerOrder.waitingTime)
            val optimizedCustomerOrders = overlappingCustomerOrders match {
              case Nil => CustomerOrder(customerOrders.head.timeToOrder, customerOrders.head.timeToCook, customerOrders.head.timeToCook) :: customerOrders.tail
              case _ => overlappingCustomerOrders.map(co => CustomerOrder(co.timeToOrder, co.timeToCook, computeWaitingTime(Some(lastProcessedCustomerOrder), co))).sortBy(_.waitingTime)
            }
            rec(Some(optimizedCustomerOrders.head), optimizedCustomerOrders.tail, optimizedCustomerOrders.head.waitingTime :: waitingTimes)
          case (None, customerOrder :: tail) =>
            val customerOrderWaitingTime = computeWaitingTime(None, customerOrder)
            rec(Some(CustomerOrder(customerOrder.timeToOrder, customerOrder.timeToCook, customerOrderWaitingTime)), tail, customerOrderWaitingTime :: waitingTimes)
        }
      }

      val waitingTimes = rec(None, sortedList, List.empty[Long])
      waitingTimes match {
        case Nil => 0
        case _ => Math.floorDiv(sumAsLong(waitingTimes), sortedList.size.toLong)
      }
    }

  private def computeWaitingTime(lastProcessedCustomerOrderOpt: Option[CustomerOrder], currentCustomerOrder: CustomerOrder):Long = {
    lastProcessedCustomerOrderOpt match {
      case None =>
        currentCustomerOrder.timeToOrder.toLong + currentCustomerOrder.timeToCook.toLong
      case Some(lastProcessedCustomerOrder) if lastProcessedCustomerOrder.waitingTime < currentCustomerOrder.timeToOrder =>
        currentCustomerOrder.timeToCook
      case Some(lastProcessedCustomerOrder) =>
        lastProcessedCustomerOrder.timeToOrder + lastProcessedCustomerOrder.waitingTime - currentCustomerOrder.timeToOrder.toLong + currentCustomerOrder.timeToCook.toLong
    }
  }

  private def sumAsLong(list: List[Long]): Long = list.foldLeft(0L)((acc, e) => acc + e)
}
