package edu.depauw.pfds.queue

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "A string" should "match itself" in {
    "hello" shouldEqual "hello"
  }
}
