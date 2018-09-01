package edu.depauw.pfds.stack

import org.scalatest._
import org.scalatest.prop.PropertyChecks

final class ListStackSpec extends PropSpec with PropertyChecks with Matchers {
  def stackToList[T](s: Stack[T]): List[T] = {
    s.top match {
      case None => Nil
      case Some(x) => x :: stackToList(s.pop.get)
    }
  }
  
  property("Contents of empty stack") {
    stackToList(ListStack()) should be (Nil)
  }
  
  property("An empty stack should be empty") {
    ListStack() should be (empty)
  }
  
  property("Top of empty") {
    ListStack().top should be (None)
  }
  
  property("Pop of empty") {
    ListStack().pop should be (None)
  }
  
  property("Contents of constructed stack") {
    forAll { (xs: List[Int]) =>
      stackToList(ListStack(xs: _*)) should be (xs)
    }
  }
  
  property("Nonempty after push") {
    forAll { (xs: List[Int], x: Int) =>
      ListStack(xs: _*).push(x) should not be (empty)
    }
  }
  
  property("Top after push") {
    forAll { (xs: List[Int], x: Int) =>
      ListStack(xs: _*).push(x).top should be (Some(x))
    }
  }
  
  property("Pop after push") {
    forAll { (xs: List[Int], x: Int) =>
      stackToList(ListStack(xs: _*).push(x).pop.get) should be (xs)
    }
  }
}