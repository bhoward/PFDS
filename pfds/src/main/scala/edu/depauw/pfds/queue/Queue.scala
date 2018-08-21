package edu.depauw.pfds.queue

trait Queue[+T] {
  def isEmpty: Boolean
  def enqueue[U >: T](x: U): Queue[U]
  def dequeue: Option[Queue[T]]
  def head: Option[T]
}