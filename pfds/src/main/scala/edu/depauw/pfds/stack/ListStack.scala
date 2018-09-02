package edu.depauw.pfds.stack

/**
 * Linked-list implementation of a stack.
 * This is a persistent data structure.
 * All operations are worst-case O(1).
 */
object ListStack {
  private class Impl[T](xs: List[T]) extends Stack[T] {
    def isEmpty: Boolean = xs.isEmpty

    def push[U >: T](x: U): Stack[U] = new Impl(x :: xs)

    def pop: Option[Stack[T]] = xs match {
      case Nil       => None
      case _ :: rest => Some(new Impl(rest))
    }

    def top: Option[T] = xs match {
      case Nil        => None
      case first :: _ => Some(first)
    }

    override def toString: String =
      xs.mkString("ListStack(", ", ", ")")
  }

  def apply[T](xs: T*): Stack[T] = new Impl(xs.toList)
}