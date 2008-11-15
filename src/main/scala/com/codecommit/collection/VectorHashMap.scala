package com.codecommit.collection

import VectorHashMap._

class VectorHashMap[K, +V] private (table: Vector[List[(K, V)]], val size: Int) extends Map[K, V] {
  
  def this() = this(allocate[K, V](10), 0)
  
  def get(key: K) = {
    def traverseBucket(bucket: List[(K, V)]): Option[V] = bucket match {
      case (k, v) :: tail => if (k == key) Some(v) else traverseBucket(tail)
      case Nil => None
    }
    
    val b = table(computeHash(key, table.length))
    if (b == null) None else traverseBucket(b)
  }
  
  override def +[A >: V](pair: (K, A)) = update(pair._1, pair._2)
  
  def update[A >: V](key: K, value: A) = {
    val (replaced, newTable) = store(grow(table, size))(key, value.asInstanceOf[V])
    new VectorHashMap[K, A](newTable, if (replaced) size else size + 1)
  }
  
  def -(key: K): VectorHashMap[K, V] = {
    val i = computeHash(key, table.length)
    val b = table(i)
    
    val (removed, newTable) = if (b == null) {
      (false, table)
    } else {
      def traverseBucket(bucket: List[(K, V)]): (Boolean, List[(K, V)]) = bucket match {
        case (k, v) :: tail => {
          if (k == key) {
            (true, tail)
          } else {
            val (found, newTail) = traverseBucket(tail)
            (found, (k, v) :: newTail)
          }
        }
        
        case Nil => (false, Nil)
      }
      
      val (found, newBucket) = traverseBucket(b)
      if (found) {
        (true, table(i) = newBucket)
      } else {
        (false, table)
      }
    }
    
    if (removed) {
      new VectorHashMap[K, V](newTable, size - 1)
    } else this
  }
  
  def elements: Iterator[(K, V)] = {
    val iterTable = table flatMap {       // quick and dirty
      case null => Vector[(K, V)]()
      
      case bucket => bucket.foldLeft(Vector[(K, V)]()) { _ + _ }
    }
    
    iterTable.elements
  }
  
  def empty[C] = new VectorHashMap[K, C]()
}

object VectorHashMap {
  
  def apply[K, V](pairs: (K, V)*) = {
    pairs.foldLeft(new VectorHashMap[K, V]()) { _ + _ }
  }
  
  @inline
  private def computeHash[K](key: K, length: Int) = Math.abs(key.hashCode % length)
  
  @inline
  private[collection] def allocate[K, V](length: Int) = {
    (0 until length).foldLeft(Vector[List[(K, V)]]()) { (v, n) => v + null }
  }
  
  @inline
  private[collection] def grow[K, V](table: Vector[List[(K, V)]], size: Int) = {
    if (size >= table.length) {
      table.foldLeft(allocate[K, V](table.length * 2)) { (table, bucket) =>
        if (bucket == null) table else {
          bucket.foldLeft(table) { (table, pair) =>
            val (key, value) = pair
            store(table)(key, value)._2
          }
        }
      }
    } else table
  }
  
  @inline
  private[collection] def store[K, V, A >: V](table: Vector[List[(K, V)]])(key: K, value: A) = {
    val i = computeHash(key, table.length)
    val b = table(i)
    
    if (b == null) {
      (false, table(i) = (key, value) :: Nil)
    } else {
      def traverseBucket(bucket: List[(K, V)]): (Boolean, List[(K, A)]) = bucket match {
        case (k, v) :: tail => {
          if (k == key) {
            (true, (key, value) :: tail)
          } else {
            val (found, newTail) = traverseBucket(tail)
            (found, (k, v) :: newTail)
          }
        }
        
        case Nil => (false, Nil)
      }
      
      val (found, newBucket) = traverseBucket(b)
      if (found) {
        (true, table(i) = newBucket)
      } else {
        (false, table(i) = ((key, value) :: b))
      }
    }
  }
}
