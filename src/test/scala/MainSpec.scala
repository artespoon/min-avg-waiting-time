import org.specs2.mutable.Specification

/**
  * Created by Piotr on 19.11.2018.
  */
class MainSpec extends Specification {

  "Test getNumberOfCustomerOrders " >> {
    "Test proper format" >> {
      Main.getNumberOfCustomerOrders("4") must_== 4
    }

    "Test proper format with spaces" >> {
      Main.getNumberOfCustomerOrders("   4   ") must_== 4
    }

    "Test empty string" >> {
      Main.getNumberOfCustomerOrders("") must throwA[NumberFormatException]
    }

    "Test wrong format" >> {
      Main.getNumberOfCustomerOrders("sdf") must throwA[NumberFormatException]
    }
  }

  "Test getCustomerOrder " >> {
    "Test proper format" >> {
      Main.getCustomerOrder("1 3") must_== CustomerOrder(1,3)
    }

    "Test proper format with multiple spaces" >> {
      Main.getCustomerOrder("1       3") must_== CustomerOrder(1,3)
    }

    "Test empty string" >> {
      Main.getCustomerOrder("") must throwA[IllegalArgumentException]
    }

    "Test wrong format" >> {
      Main.getCustomerOrder("1 sdf") must throwA[IllegalArgumentException]
    }
  }


}
