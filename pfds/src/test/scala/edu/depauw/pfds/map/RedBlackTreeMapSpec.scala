package edu.depauw.pfds.map

import org.scalatest._
import org.scalatest.prop.PropertyChecks

final class RedBlackTreeMapSpec extends PropSpec with PropertyChecks with Matchers {
  property("Get from empty") {
    forAll { (n: Int) =>
      RedBlackTreeMap[Int, Nothing]().get(n) should be(None)
    }
  }

  property("Get after put") {
    forAll { (xs: List[(Int, String)], key: Int, value: String) =>
      RedBlackTreeMap(xs: _*).put(key, value).get(key) should be(Some(value))
    }
  }

  property("Get after non-conflicting puts") {
    forAll { (xs: List[(Int, String)], key: Int, value: String, ys: List[(Int, String)]) =>
      val keys = ys.map(_._1)
      if (!keys.contains(key)) {
        val map = ys.foldLeft(RedBlackTreeMap(xs: _*).put(key, value)) {
          case (m, (key, value)) => m.put(key, value)
        }
        map.get(key) should be(Some(value))
      }
    }
  }
}