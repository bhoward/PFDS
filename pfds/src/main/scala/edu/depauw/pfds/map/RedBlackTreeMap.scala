package edu.depauw.pfds.map

/**
 * Red-Black Binary Search Tree implementation of map.
 * Persistent data structure, with guaranteed worst-case
 * O(log N) get and put.
 * Source: Okasaki1998
 */
object RedBlackTreeMap {
  private class Impl[K, +V](t: RBT[K, V]) extends Map[K, V] {
    def get(key: K): Option[V] = t.get(key)
    def put[U >: V](key: K, value: U): Map[K, U] = new Impl(t.put(key, value).blacken)
  }

  private trait RBT[K, +V] {
    def get(key: K): Option[V]
    def put[U >: V](key: K, value: U): RBT[K, U]
    def blacken: RBT[K, V]
  }

  private case class Empty[K]()(implicit ord: Ordering[K]) extends RBT[K, Nothing] {
    def get(key: K): Option[Nothing] = None

    def put[V](key: K, value: V): RBT[K, V] = RedNode(this, key, value, this)

    def blacken: RBT[K, Nothing] = this
  }

  private case class RedNode[K, V](left: RBT[K, V], k: K, v: V, right: RBT[K, V])(implicit ord: Ordering[K]) extends RBT[K, V] {
    def get(key: K): Option[V] = {
      if (ord.lt(key, k)) {
        left.get(key)
      } else if (ord.lt(k, key)) {
        right.get(key)
      } else {
        Some(v)
      }
    }

    def put[U >: V](key: K, value: U): RBT[K, U] = {
      if (ord.lt(key, k)) {
        RedNode(left.put(key, value), k, v, right)
      } else if (ord.lt(k, key)) {
        RedNode(left, k, v, right.put(key, value))
      } else {
        RedNode(left, key, value, right)
      }
    }

    def blacken: RBT[K, V] = BlackNode(left, k, v, right)
  }

  private case class BlackNode[K, V](left: RBT[K, V], k: K, v: V, right: RBT[K, V])(implicit ord: Ordering[K]) extends RBT[K, V] {
    def get(key: K): Option[V] = {
      if (ord.lt(key, k)) {
        left.get(key)
      } else if (ord.lt(k, key)) {
        right.get(key)
      } else {
        Some(v)
      }
    }

    def put[U >: V](key: K, value: U): RBT[K, U] = {
      if (ord.lt(key, k)) {
        BlackNode(left.put(key, value), k, v, right).balance
      } else if (ord.lt(k, key)) {
        BlackNode(left, k, v, right.put(key, value)).balance
      } else {
        BlackNode(left, key, value, right)
      }
    }

    def blacken: RBT[K, V] = this

    private def balance: RBT[K, V] = (left, right) match {
      case (RedNode(RedNode(lll, llk, llv, llr), lk, lv, lr), r) =>
        RedNode(BlackNode(lll, llk, llv, llr), lk, lv, BlackNode(lr, k, v, r))
      case (RedNode(ll, lk, lv, RedNode(lrl, lrk, lrv, lrr)), r) =>
        RedNode(BlackNode(ll, lk, lv, lrl), lrk, lrv, BlackNode(lrr, k, v, r))
      case (l, RedNode(RedNode(rll, rlk, rlv, rlr), rk, rv, rr)) =>
        RedNode(BlackNode(l, k, v, rll), rlk, rlv, BlackNode(rlr, rk, rv, rr))
      case (l, RedNode(rl, rk, rv, RedNode(rrl, rrk, rrv, rrr))) =>
        RedNode(BlackNode(l, k, v, rl), rk, rv, BlackNode(rrl, rrk, rrv, rrr))
      case _ => this
    }
  }

  def apply[K, V](ps: (K, V)*)(implicit ord: Ordering[K]): Map[K, V] = {
    new Impl(ps.foldLeft[RBT[K, V]](Empty()) {
      case (t, (key, value)) => t.put(key, value)
    })
  }
}