package edu.depauw.pfds.priorityqueue

import edu.depauw.pfds.instrumentation.Recorder

/**
 * Simpler, self-adjusting relative of leftist heap, with only amortized
 * O(log N) guarantees on enqueue/dequeue.
 *
 * Instrumented with a Recorder to evaluate run time.
 */
object InstrumentedSkewHeap {
  private class Impl[T](t: Heap[T])(implicit recorder: Recorder) extends PriorityQueue[T] {
    def isEmpty: Boolean = recorder.op(this) {
      t.isEmpty
    }

    def enqueue(x: T): PriorityQueue[T] = recorder.op(this) {
      new Impl(t.enqueue(x))
    }

    def dequeue: Option[PriorityQueue[T]] = recorder.op(this) {
      t.dequeue.map(new Impl(_))
    }

    def head: Option[T] = recorder.op(this) {
      t.head
    }
    
    override def toString: String = t.toString
  }

  private trait Heap[T] {
    def isEmpty(implicit recorder: Recorder): Boolean
    def enqueue(x: T)(implicit recorder: Recorder): Heap[T]
    def dequeue(implicit recorder: Recorder): Option[Heap[T]]
    def head(implicit recorder: Recorder): Option[T]
    def merge(t: Heap[T])(implicit recorder: Recorder): Heap[T]
  }

  private case class Empty[T]()(implicit ord: Ordering[T]) extends Heap[T] {
    def isEmpty(implicit recorder: Recorder): Boolean = recorder.tick {
      true
    }

    def enqueue(x: T)(implicit recorder: Recorder): Heap[T] = recorder.tick {
      Node(this, x, this)
    }

    def dequeue(implicit recorder: Recorder): Option[Heap[T]] = recorder.tick {
      None
    }

    def head(implicit recorder: Recorder): Option[T] = recorder.tick {
      None
    }

    def merge(t: Heap[T])(implicit recorder: Recorder): Heap[T] = recorder.tick {
      t
    }
    
    override def toString: String = "."
  }

  private case class Node[T](left: Heap[T], v: T, right: Heap[T])(implicit ord: Ordering[T]) extends Heap[T] {
    def isEmpty(implicit recorder: Recorder): Boolean = recorder.tick {
      false
    }

    def enqueue(x: T)(implicit recorder: Recorder): Heap[T] = recorder.tick {
      merge(Node(Empty(), x, Empty()))
    }

    def dequeue(implicit recorder: Recorder): Option[Heap[T]] = recorder.tick {
      Some(left.merge(right))
    }

    def head(implicit recorder: Recorder): Option[T] = recorder.tick {
      Some(v)
    }

    def merge(t: Heap[T])(implicit recorder: Recorder): Heap[T] = recorder.tick {
      t match {
        case Empty() => this
        case Node(left2, v2, right2) =>
          if (ord.lt(v, v2)) {
            Node(right.merge(t), v, left)
          } else {
            Node(right2.merge(this), v2, left2)
          }
      }
    }
    
    override def toString: String = s"($left $v $right)"
  }

  def min[T](xs: T*)(implicit ord: Ordering[T], recorder: Recorder): PriorityQueue[T] = {
    new Impl(xs.foldLeft[Heap[T]](Empty()) {
      case (t, x) => recorder.op(x) {
        t.enqueue(x)
      }
    })
  }

  def max[T](xs: T*)(implicit ord: Ordering[T], recorder: Recorder): PriorityQueue[T] = {
    new Impl(xs.foldLeft[Heap[T]](Empty()(ord.reverse)) {
      case (t, x) => recorder.op(x) {
        t.enqueue(x)
      }
    })
  }
}