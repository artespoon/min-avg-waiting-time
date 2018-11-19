import org.specs2.mutable.Specification

/**
  * Created by Piotr on 19.11.2018.
  */
class OrdersProcessorSpec extends Specification {
  "Test getAvgWaitingTime " >> {

    "Test with empty list" >> {
      OrdersProcessor.getAvgWaitingTime(List()) must_== 0
    }

    "Test with proper list of customer orders" >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,3), CustomerOrder(1,9), CustomerOrder(2,6))) must_== 9
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,3), CustomerOrder(1,9), CustomerOrder(2,5))) must_== 8
    }

    "Test with list of customers in wrong order " >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(1,9), CustomerOrder(2,6), CustomerOrder(0,3))) must_== 9
    }

    "Test with all orders at the same time " >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,3), CustomerOrder(0,3), CustomerOrder(0,3))) must_== 6
    }

    "Test with NOT overlapping orders" >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,1), CustomerOrder(10,3))) must_== 2
    }

    "Test with NOT overlapping orders 1" >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,1), CustomerOrder(10,2), CustomerOrder(20,3))) must_== 2
    }

    "Test with time to order with Int max value" >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,1), CustomerOrder(Int.MaxValue,1))) must_== 1
    }

    "Test with time to order with Int max value 1" >> {
      OrdersProcessor.getAvgWaitingTime(List(CustomerOrder(0,1), CustomerOrder(3, Int.MaxValue), CustomerOrder(Int.MaxValue,1))) must_== 715827884 //Math.floorDiv(1L + Int.MaxValue + 4L, 3L)
    }
  }
}
