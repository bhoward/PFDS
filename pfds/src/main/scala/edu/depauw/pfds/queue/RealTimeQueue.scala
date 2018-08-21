package edu.depauw.pfds.queue

import scala.Stream
import scala.collection.immutable.Stream.consWrapper

/**
 * Pair-of-linked-lists-plus-laziness implementation of a queue.
 * This is a persistent data structure.
 * All operations are worst-case O(1).
 * Source: Okasaki1998
 */
object RealTimeQueue {
  private class Impl[T](front: Stream[T], rear: List[T], sched: Stream[T]) extends Queue[T] {
    // The contents of the queue are front ++ rear.reverse
    // Invariant: sched.length == front.length - rear.length
    
    def isEmpty: Boolean = front.isEmpty

    def enqueue[U >: T](x: U): Queue[U] = new Impl(front, x :: rear, sched).exec

    def dequeue: Option[Queue[T]] = front match {
      case Stream.Empty => None
      case _ #:: rest   => Some(new Impl(rest, rear, sched).exec)
    }

    def head: Option[T] = front match {
      case Stream.Empty => None
      case first #:: _  => Some(first)
    }

    // rotate(f, r, a) = f ++ r.reverse ++ a, done incrementally as a stream
    // Invariant: r.length == f.length + 1
    private def rotate(f: Stream[T], r: List[T], a: Stream[T]): Stream[T] = (f, r) match {
      case (Stream.Empty, y :: _) => y #:: a
      case (x #:: xs, y :: ys)    => x #:: rotate(xs, ys, y #:: a)
    }

    // Execute one step of the incremental rotation, according to the "schedule".
    // sched.length == front.length - rear.length + 1 when called
    private def exec: Impl[T] = sched match {
      case Stream.Empty => {
        val s = rotate(front, rear, Stream.Empty)
        new Impl(s, Nil, s)
      }

      case _ #:: tail => new Impl(front, rear, tail)
    }

    override def toString: String =
      (front ++ rear.reverse).mkString("RealTimeQueue(", ", ", ")")
  }

  def apply[T](xs: T*): Queue[T] = {
    val s = xs.toStream
    new Impl(s, Nil, s)
  }
}