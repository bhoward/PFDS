package edu.depauw.pfds.map

/**
 * Binary Search Tree implementation of map.
 * Persistent data structure, but tree may become unbalanced
 * so only guarantees average case O(log N) get and put.
 */
object BSTMap {
  private trait BST[K, +V] extends Map[K, V] {
    // specialize the return type of put:
    def put[U >: V](key: K, value: U): BST[K, U]
  }

  private case class Empty[K]()(implicit ord: Ordering[K]) extends BST[K, Nothing] {
    def get(key: K): Option[Nothing] = None

    def put[V](key: K, value: V): BST[K, V] = Node(this, key, value, this)
  }

  private case class Node[K, V](left: BST[K, V], k: K, v: V, right: BST[K, V])(implicit ord: Ordering[K]) extends BST[K, V] {
    def get(key: K): Option[V] = {
      if (ord.lt(key, k)) {
        left.get(key)
      } else if (ord.lt(k, key)) {
        right.get(key)
      } else {
        Some(v)
      }
    }

    def put[U >: V](key: K, value: U): BST[K, U] = {
      if (ord.lt(key, k)) {
        Node(left.put(key, value), k, v, right)
      } else if (ord.lt(k, key)) {
        Node(left, k, v, right.put(key, value))
      } else {
        Node(left, key, value, right)
      }
    }
  }

  def apply[K, V](ps: (K, V)*)(implicit ord: Ordering[K]): Map[K, V] = {
    ps.foldLeft[BST[K, V]](Empty()) {
      case (t, (key, value)) => t.put(key, value)
    }
  }
}