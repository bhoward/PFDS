package edu.depauw.pfds.priorityqueue

/**
 * Leftist heap: left child always has rank (= length of right spine)
 * at least as large as right child. Guarantees O(log N) enqueue/dequeue,
 * and O(1) head. Persistent data structure.
 * Source: Okasaki1998
 */
object LeftistHeap {
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

    def enqueue(x: T): Heap[T] = Node(1, this, x, this)

    def dequeue: Option[Heap[T]] = None

    def head: Option[T] = None
    
    def merge(t: Heap[T]): Heap[T] = t
  }

  private case class Node[T](rank: Int, left: Heap[T], v: T, right: Heap[T])(implicit ord: Ordering[T]) extends Heap[T] {
    def isEmpty: Boolean = false
    
    def enqueue(x: T): Heap[T] = merge(Node(1, Empty(), x, Empty()))
    
    def dequeue: Option[Heap[T]] = Some(left.merge(right))
    
    def head: Option[T] = Some(v)
    
    def merge(t: Heap[T]): Heap[T] = t match {
      case Empty() => this
      case Node(_, left2, v2, right2) =>
        if (ord.lt(v, v2)) {
          makeNode(left, v, right.merge(t))
        } else {
          makeNode(left2, v2, right2.merge(this))
        }
    }
  }

  private def rank[T](t: Heap[T]): Int = t match {
    case Empty() => 0
    case Node(r, _, _, _) => r
  }
  
  private def makeNode[T](a: Heap[T], v: T, b: Heap[T])(implicit ord: Ordering[T]): Heap[T] = {
    if (rank(a) >= rank(b)) {
      Node(rank(b) + 1, a, v, b)
    } else {
      Node(rank(a) + 1, b, v, a)
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