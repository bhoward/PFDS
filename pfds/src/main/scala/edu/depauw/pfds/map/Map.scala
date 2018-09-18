package edu.depauw.pfds.map

trait Map[K, +V] {
  def get(key: K): Option[V]
  def put[U >: V](key: K, value: U): Map[K, U]
}