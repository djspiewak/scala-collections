package com.codecommit.collection

class VectorHashMap[K, +V] private (table: Vector[List[(K, V)]], val size: Int) extends Map[K, V] {
  
  def this() = this(Vector[List[(K, V)]](), 0)
  
  def get(key: K) = {
    def traverseBucket(bucket: List[(K, V)]): Option[V] = bucket match {
      case (k, v) :: tail => if (k == key) Some(v) else traverseBucket(tail)
      case Nil => None
    }
    
    val b = table(computeHash(key))
    if (b == null) None else traverseBucket(b)
  }
  
  override def +[A >: V](pair: (K, A)) = update(pair._1, pair._2)
  
  def update[A >: V](key: K, value: A) = {
    val i = computeHash(key)
    val b = table(i)
    
    val (replaced, newTable) = if (b == null) {
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
    
    new VectorHashMap[K, A](newTable, if (replaced) size else size + 1)
  }
  
  def -(key: K): VectorHashMap[K, V] = {
    val i = computeHash(key)
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
  
  @inline
  private def computeHash(key: K) = key.hashCode % table.length
}
