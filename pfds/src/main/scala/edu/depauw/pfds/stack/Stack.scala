package edu.depauw.pfds.stack

trait Stack[+T] {
  def isEmpty: Boolean
  def push[U >: T](x: U): Stack[U]
  def pop: Option[Stack[T]]
  def top: Option[T]
}