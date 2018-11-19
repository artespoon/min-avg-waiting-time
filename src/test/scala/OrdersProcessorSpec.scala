import org.specs2.mutable.Specification

/**
  * Created by Piotr on 19.11.2018.
  */
class OrdersProcessorSpec extends Specification {
  "Test getAvgWaitingTime " >> {

    "Test with empty list" >> {
      OrdersProcessor.getAvgWaitingTime(List()) must_== 0
    }

    "Test with proper list with customer orders" >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,3), CustomerOrder(1,9), CustomerOrder(2,6))) must_== 9
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,3), CustomerOrder(1,9), CustomerOrder(2,5))) must_== 8
    }

    "Test with list in wrong order " >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(1,9), CustomerOrder(2,6), CustomerOrder(0,3))) must_== 9
    }

    "Test with all orders at the same time " >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,3), CustomerOrder(0,3), CustomerOrder(0,3))) must_== 6
    }
  }
}
