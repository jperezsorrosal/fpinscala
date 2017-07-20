package fpinscala.gettingstarted

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by jperezsl on 31/1/16.
  */
class PolymorphicFunctions$Test extends FunSuite with Matchers {
  import PolymorphicFunctions._

  test("testIsSorted") {
    val sorted = Array(1, 2, 3, 6, 74, 432, 23424)
    val unsorted = Array ( 234, 13, 4462, 2, 123)

    isSorted[Int](sorted, {_ <= _}) should equal (true)
    isSorted[Int](unsorted, {_ <= _}) should equal (false)

    isSorted2[Int](sorted, {_ <= _}) should equal (true)
    isSorted2[Int](unsorted, {_ <= _}) should equal (false)
  }

}

