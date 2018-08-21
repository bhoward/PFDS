package edu.depauw.pfds.queue

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class HelloSpec extends FlatSpec with Matchers {
  "A string" should "match itself" in {
    "hello" shouldEqual "hello"
  }
}

final class StringSpec extends PropSpec with PropertyChecks with Matchers {
  property("startsWith") {
    forAll { (a: String, b: String) =>
      (a+b) should startWith (a)
    }
  }

  property("concatenate") {
    forAll { (a: String, b: String) =>
      (a+b).length should (be >= a.length)
      (a+b).length should (be >= b.length)
    }
  }

  property("substring") {
    forAll { (a: String, b: String, c: String) =>
      (a+b+c).substring(a.length, a.length+b.length) should be (b)
    }
  }
}