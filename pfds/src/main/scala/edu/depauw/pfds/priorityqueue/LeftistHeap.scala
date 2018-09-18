package edu.depauw.pfds.priorityqueue

object LeftistHeap {
  private class Impl[T]() extends PriorityQueue[T] {
    def isEmpty: Boolean = ???
    
    def enqueue(x: T): PriorityQueue[T] = ???
    
    def dequeue: Option[PriorityQueue[T]] = ???
    
    def head: Option[T] = ???
  }
  
  def apply[T](xs: T*): PriorityQueue[T] = new Impl()
}