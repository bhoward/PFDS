package edu.depauw.pfds.priorityqueue

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

import edu.depauw.pfds.instrumentation.Recorder

final class InstrumentedSkewHeapSpec extends PropSpec with PropertyChecks with Matchers {
  def queueToList[T](q: PriorityQueue[T]): List[T] = {
    q.head match {
      case None    => Nil
      case Some(x) => x :: queueToList(q.dequeue.get)
    }
  }

  property("Contents of empty queue") {
    implicit val recorder = new Recorder

    queueToList(InstrumentedSkewHeap.min[Int]()) should be(Nil)

    recorder.maximumTicksPerOp should be(1)
  }

  property("An empty queue should be empty") {
    implicit val recorder = new Recorder

    InstrumentedSkewHeap.min[Int]() should be(empty)

    recorder.maximumTicksPerOp should be(1)
  }

  property("Head of empty") {
    implicit val recorder = new Recorder

    InstrumentedSkewHeap.min[Int]().head should be(None)

    recorder.maximumTicksPerOp should be(1)
  }

  property("Dequeue of empty") {
    implicit val recorder = new Recorder

    InstrumentedSkewHeap.min[Int]().dequeue should be(None)

    recorder.maximumTicksPerOp should be(1)
  }

  property("Contents of constructed min queue") {
    forAll { (xs: List[Int]) =>
      implicit val recorder = new Recorder

      queueToList(InstrumentedSkewHeap.min(xs: _*)) should be(xs.sorted)

      recorder.maximumAverageTicksPerOp + 0.0 should (be <= math.log(xs.size + 1.0) / math.log(1.62) + 1)
    }
  }

  property("Min Queue creates order") {
    forAll { (xs: List[Int]) =>
      implicit val recorder = new Recorder

      var q = InstrumentedSkewHeap.min[Int]()
      for (x <- xs) {
        q = q.enqueue(x)
      }
      queueToList(q) should be(xs.sorted)

      recorder.maximumAverageTicksPerOp + 0.0 should (be <= math.log(xs.size + 1.0) / math.log(1.62) + 1)
    }
  }

  property("Contents of constructed max queue") {
    forAll { (xs: List[Int]) =>
      implicit val recorder = new Recorder

      queueToList(InstrumentedSkewHeap.max(xs: _*)) should be(xs.sorted.reverse)

      recorder.maximumAverageTicksPerOp + 0.0 should (be <= math.log(xs.size + 1.0) / math.log(1.62) + 1)
    }
  }

  property("Max Queue creates order") {
    forAll { (xs: List[Int]) =>
      implicit val recorder = new Recorder

      var q = InstrumentedSkewHeap.max[Int]()
      for (x <- xs) {
        q = q.enqueue(x)
      }
      queueToList(q) should be(xs.sorted.reverse)

      recorder.maximumAverageTicksPerOp + 0.0 should (be <= math.log(xs.size + 1.0) / math.log(1.62) + 1)
    }
  }

  property("Nonempty after enqueue") {
    forAll { (xs: List[Int], x: Int) =>
      implicit val recorder = new Recorder

      InstrumentedSkewHeap.min(xs: _*).enqueue(x) should not be (empty)

      recorder.maximumAverageTicksPerOp + 0.0 should (be <= math.log(xs.size + 1.0) / math.log(1.62) + 1)
    }
  }

  val sizes = for (n <- Gen.choose(10, 10000)) yield n

  property("Skew Heap is amortized O(log N)") {
    forAll(sizes) { (n: Int) =>
      whenever(10 <= n && n <= 10000) {
        implicit val recorder = new Recorder
        var q: PriorityQueue[Int] = InstrumentedSkewHeap.min()

        val nums = scala.util.Random.shuffle(1 to n)

        for (i <- nums) {
          q = q.enqueue(i)
        }

        q.head should be(Some(1))

        for (_ <- 1 to n) {
          val q2 = q.dequeue.get
          q2.head should be(Some(2))
        }

        recorder.maximumAverageTicksPerOp + 0.0 should (be <= math.log(n + 1.0) / math.log(1.62) + 1)
      }
    }
  }

}