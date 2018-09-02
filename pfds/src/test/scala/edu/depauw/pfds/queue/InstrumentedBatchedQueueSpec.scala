package edu.depauw.pfds.queue

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import edu.depauw.pfds.instrumentation.Recorder

final class InstrumentedBatchedQueueSpec extends PropSpec with PropertyChecks with Matchers {
  property("Queue should deliver all values in amortized constant time") {
    implicit val recorder = new Recorder
    var q: Queue[Int] = InstrumentedBatchedQueue()
    
    for (i <- 1 to 1000) {
      q = q.enqueue(i)
    }
    
    for (i <- 1 to 1000) {
      q.head should be (Some(i))
      q = q.dequeue.get
    }
    
    recorder.maximumAverageTicksPerOp should (be <= 2)
  }
}