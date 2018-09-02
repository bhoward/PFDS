package edu.depauw.pfds.queue

import org.scalatest._
import org.scalatest.prop.PropertyChecks

final class BatchedQueueSpec extends PropSpec with PropertyChecks with Matchers {
  def queueToList[T](q: Queue[T]): List[T] = {
    q.head match {
      case None => Nil
      case Some(x) => x :: queueToList(q.dequeue.get)
    }
  }
  
  property("Contents of empty queue") {
    queueToList(BatchedQueue()) should be (Nil)
  }
  
  property("An empty queue should be empty") {
    BatchedQueue() should be (empty)
  }
  
  property("Head of empty") {
    BatchedQueue().head should be (None)
  }
  
  property("Dequeue of empty") {
    BatchedQueue().dequeue should be (None)
  }
  
  property("Contents of constructed queue") {
    forAll { (xs: List[Int]) =>
      queueToList(BatchedQueue(xs: _*)) should be (xs)
    }
  }
  
  property("Queue preserves order") {
    forAll { (xs: List[Int]) =>
      var q = BatchedQueue[Int]()
      for (x <- xs) {
        q = q.enqueue(x)
      }
      queueToList(q) should be (xs)
    }
  }
  
  property("Nonempty after enqueue") {
    forAll { (xs: List[Int], x: Int) =>
      BatchedQueue(xs: _*).enqueue(x) should not be (empty)
    }
  }
}