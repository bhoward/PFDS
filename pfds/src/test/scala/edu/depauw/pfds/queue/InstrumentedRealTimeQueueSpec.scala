package edu.depauw.pfds.queue

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import edu.depauw.pfds.instrumentation.Recorder

final class InstrumentedRealTimeQueueSpec extends PropSpec with PropertyChecks with Matchers {
  property("Queue should deliver all values in constant time") {
    val recorder = new Recorder
    var q: Queue[Int] = InstrumentedRealTimeQueue()(recorder)
    
    for (i <- 1 to 1000) {
      q = q.enqueue(i)
    }
    
    for (i <- 1 to 1000) {
      q.head.get should be (i)
      q = q.dequeue.get
    }
    
    recorder.maximumTicksPerOp should (be <= 2)
  }
}