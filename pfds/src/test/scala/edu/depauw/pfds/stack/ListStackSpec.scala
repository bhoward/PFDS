package edu.depauw.pfds.stack

import org.scalatest._
import org.scalatest.prop.PropertyChecks

final class ListStackSpec extends PropSpec with PropertyChecks with Matchers {
  property("An empty stack should be empty") {
    ListStack() should be (empty)
  }
  
  property("startsWith") {
    forAll { (a: String, b: String) =>
      (a+b) should startWith (a)
    }
  }
}