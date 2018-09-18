package edu.depauw.pfds.priorityqueue

import org.scalatest._
import org.scalatest.prop.PropertyChecks

final class SkewHeapSpec extends PropSpec with PropertyChecks with Matchers {
  def queueToList[T](q: PriorityQueue[T]): List[T] = {
    q.head match {
      case None => Nil
      case Some(x) => x :: queueToList(q.dequeue.get)
    }
  }
  
  property("Contents of empty queue") {
    queueToList(SkewHeap.min[Int]()) should be (Nil)
  }
  
  property("An empty queue should be empty") {
    SkewHeap.min[Int]() should be (empty)
  }
  
  property("Head of empty") {
    SkewHeap.min[Int]().head should be (None)
  }
  
  property("Dequeue of empty") {
    SkewHeap.min[Int]().dequeue should be (None)
  }
  
  property("Contents of constructed min queue") {
    forAll { (xs: List[Int]) =>
      queueToList(SkewHeap.min(xs: _*)) should be (xs.sorted)
    }
  }
  
  property("Min Queue creates order") {
    forAll { (xs: List[Int]) =>
      var q = SkewHeap.min[Int]()
      for (x <- xs) {
        q = q.enqueue(x)
      }
      queueToList(q) should be (xs.sorted)
    }
  }
  
  property("Contents of constructed max queue") {
    forAll { (xs: List[Int]) =>
      queueToList(SkewHeap.max(xs: _*)) should be (xs.sorted.reverse)
    }
  }
  
  property("Max Queue creates order") {
    forAll { (xs: List[Int]) =>
      var q = SkewHeap.max[Int]()
      for (x <- xs) {
        q = q.enqueue(x)
      }
      queueToList(q) should be (xs.sorted.reverse)
    }
  }
  
  property("Nonempty after enqueue") {
    forAll { (xs: List[Int], x: Int) =>
      SkewHeap.min(xs: _*).enqueue(x) should not be (empty)
    }
  }
}