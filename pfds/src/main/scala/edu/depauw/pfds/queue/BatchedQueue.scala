package edu.depauw.pfds.queue

/**
 * Pair-of-linked-lists implementation of a queue.
 * This is _not_ a persistent data structure.
 * isEmpty, head, and enqueue are worst-case O(1);
 * dequeue is amortized O(1).
 * Source: Okasaki1998
 */
object BatchedQueue {
  private class BatchedQueue[T](front: List[T], rear: List[T]) extends Queue[T] {
    // The contents of the queue are front ++ rear.reverse
    // Invariant: if front is empty, then so is rear
    def isEmpty: Boolean = front.isEmpty

    def enqueue[U >: T](x: U): Queue[U] = new BatchedQueue(front, x :: rear).fix

    def dequeue: Option[Queue[T]] = front match {
      case Nil       => None
      case _ :: rest => Some(new BatchedQueue(rest, rear).fix)
    }

    def head: Option[T] = front match {
      case Nil        => None
      case first :: _ => Some(first)
    }

    private def fix: BatchedQueue[T] = front match {
      case Nil => new BatchedQueue(rear.reverse, Nil)
      case _   => this
    }

    override def toString: String =
      (front ++ rear.reverse).mkString("BatchedQueue(", ", ", ")")
  }

  def apply[T](xs: T*): Queue[T] = new BatchedQueue(xs.toList, Nil)
}