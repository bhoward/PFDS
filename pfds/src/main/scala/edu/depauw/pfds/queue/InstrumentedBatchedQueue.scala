package edu.depauw.pfds.queue

import edu.depauw.pfds.instrumentation.Recorder

/**
 * Pair-of-linked-lists implementation of a queue.
 * This is _not_ a persistent data structure.
 * isEmpty, head, and enqueue are worst-case O(1);
 * dequeue is amortized O(1).
 * Source: Okasaki1998
 *
 * Instrumented with a Recorder to check run time.
 */
object InstrumentedBatchedQueue {
  private class Impl[T](front: List[T], rear: List[T])(implicit recorder: Recorder) extends Queue[T] {
    // The contents of the queue are front ++ rear.reverse
    // Invariant: if front is empty, then so is rear
    def isEmpty: Boolean = front.isEmpty

    def enqueue[U >: T](x: U): Queue[U] = recorder.op {
      new Impl(front, x :: rear).fix
    }

    def dequeue: Option[Queue[T]] = recorder.op {
      front match {
        case Nil       => None
        case _ :: rest => Some(new Impl(rest, rear).fix)
      }
    }

    def head: Option[T] = recorder.op {
      front match {
        case Nil        => None
        case first :: _ => Some(first)
      }
    }

    private def fix: Impl[T] = front match {
      case Nil => recorder.ticks(rear.size) {
        new Impl(rear.reverse, Nil)
      }
      case _   => recorder.tick {
        this
      }
    }

    override def toString: String =
      (front ++ rear.reverse).mkString("BatchedQueue(", ", ", ")")
  }

  def apply[T](xs: T*)(implicit recorder: Recorder): Queue[T] =
    new Impl(xs.toList, Nil)
}