package edu.depauw.pfds.queue

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

import edu.depauw.pfds.instrumentation.Recorder

final class InstrumentedBatchedQueueSpec extends PropSpec with PropertyChecks with Matchers {
  property("Batched Queue should deliver all values in amortized constant time") {
    implicit val recorder = new Recorder
    var q: Queue[Int] = InstrumentedBatchedQueue()

    for (i <- 1 to 1000) {
      q = q.enqueue(i)
    }

    for (i <- 1 to 1000) {
      q.head should be(Some(i))
      q = q.dequeue.get
    }

    recorder.maximumAverageTicksPerOp should (be <= 2)
  }

  val sizes = for (n <- Gen.choose(10, 10000)) yield n

  property("Batched Queue is not persistent") {
    forAll(sizes) { (n: Int) =>
      whenever(10 <= n && n <= 10000) {
        implicit val recorder = new Recorder
        var q: Queue[Int] = InstrumentedBatchedQueue()

        for (i <- 1 to n) {
          q = q.enqueue(i)
        }

        for (_ <- 1 to n) {
          val q2 = q.dequeue.get
          q2.head should be(Some(2))
        }

        recorder.maximumAverageTicksPerOp should (be > n / 4)
      }
    }
  }
}