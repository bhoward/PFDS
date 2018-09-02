package edu.depauw.pfds.queue

import org.scalatest._
import org.scalatest.prop.PropertyChecks

final class RealTimeQueueSpec extends PropSpec with PropertyChecks with Matchers {
  def queueToList[T](q: Queue[T]): List[T] = {
    q.head match {
      case None => Nil
      case Some(x) => x :: queueToList(q.dequeue.get)
    }
  }
  
  property("Contents of empty queue") {
    queueToList(RealTimeQueue()) should be (Nil)
  }
  
  property("An empty queue should be empty") {
    RealTimeQueue() should be (empty)
  }
  
  property("Head of empty") {
    RealTimeQueue().head should be (None)
  }
  
  property("Dequeue of empty") {
    RealTimeQueue().dequeue should be (None)
  }
  
  property("Contents of constructed queue") {
    forAll { (xs: List[Int]) =>
      queueToList(RealTimeQueue(xs: _*)) should be (xs)
    }
  }
  
  property("Queue preserves order") {
    forAll { (xs: List[Int]) =>
      var q = RealTimeQueue[Int]()
      for (x <- xs) {
        q = q.enqueue(x)
      }
      queueToList(q) should be (xs)
    }
  }
  
  property("Nonempty after enqueue") {
    forAll { (xs: List[Int], x: Int) =>
      RealTimeQueue(xs: _*).enqueue(x) should not be (empty)
    }
  }
}