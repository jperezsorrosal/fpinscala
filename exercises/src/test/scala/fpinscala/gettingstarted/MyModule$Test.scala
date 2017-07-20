package fpinscala.gettingstarted

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by jperezsl on 31/1/16.
  */
class MyModule$Test extends FunSuite with Matchers{

  import MyModule._

  test("testFib") {
    fib(0) should equal (0)
    fib(1) should equal (1)
    fib(2) should equal (1)
    fib(3) should equal (2)
    fib(4) should equal (3)
    fib(12) should equal (144)
  }

}
