package edu.depauw.pfds.priorityqueue

trait PriorityQueue[T] {
  def isEmpty: Boolean
  def enqueue(x: T): PriorityQueue[T]
  def dequeue: Option[PriorityQueue[T]]
  def head: Option[T]
}