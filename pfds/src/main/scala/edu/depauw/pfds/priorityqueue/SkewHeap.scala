package edu.depauw.pfds.priorityqueue

/**
 * Simpler, self-adjusting relative of leftist heap, with only amortized
 * O(log N) guarantees on enqueue/dequeue.
 */
object SkewHeap {
  private class Impl[T](t: Heap[T]) extends PriorityQueue[T] {
    def isEmpty: Boolean = t.isEmpty

    def enqueue(x: T): PriorityQueue[T] = new Impl(t.enqueue(x))

    def dequeue: Option[PriorityQueue[T]] = t.dequeue.map(new Impl(_))

    def head: Option[T] = t.head
  }

  private trait Heap[T] {
    def isEmpty: Boolean
    def enqueue(x: T): Heap[T]
    def dequeue: Option[Heap[T]]
    def head: Option[T]
    def merge(t: Heap[T]): Heap[T]
  }

  private case class Empty[T]()(implicit ord: Ordering[T]) extends Heap[T] {
    def isEmpty: Boolean = true

    def enqueue(x: T): Heap[T] = Node(this, x, this)

    def dequeue: Option[Heap[T]] = None

    def head: Option[T] = None

    def merge(t: Heap[T]): Heap[T] = t
  }

  private case class Node[T](left: Heap[T], v: T, right: Heap[T])(implicit ord: Ordering[T]) extends Heap[T] {
    def isEmpty: Boolean = false

    def enqueue(x: T): Heap[T] = merge(Node(Empty(), x, Empty()))

    def dequeue: Option[Heap[T]] = Some(left.merge(right))

    def head: Option[T] = Some(v)

    def merge(t: Heap[T]): Heap[T] = t match {
      case Empty() => this
      case Node(left2, v2, right2) =>
        if (ord.lt(v, v2)) {
          Node(right.merge(t), v, left)
        } else {
          Node(right2.merge(this), v2, left2)
        }
    }
  }

  def min[T](xs: T*)(implicit ord: Ordering[T]): PriorityQueue[T] = {
    new Impl(xs.foldLeft[Heap[T]](Empty()) {
      case (t, x) => t.enqueue(x)
    })
  }

  def max[T](xs: T*)(implicit ord: Ordering[T]): PriorityQueue[T] = {
    new Impl(xs.foldLeft[Heap[T]](Empty()(ord.reverse)) {
      case (t, x) => t.enqueue(x)
    })
  }
}