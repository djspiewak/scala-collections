package com.codecommit.collection

import HashMap._

class HashMap[K, +V] private (root: Node[K, V]) extends Map[K, V] {
  lazy val size = root.size
  
  def this() = this(new EmptyNode[K])
  
  def get(key: K) = root(key, key.hashCode)
  
  override def +[A >: V](pair: (K, A)) = pair match {
    case (k, v) => update(k, v)
  }
  
  def update[A >: V](key: K, value: A) = new HashMap(root(key, key.hashCode) = value)
  
  def -(key: K) = new HashMap(root.remove(key, key.hashCode))
  
  def elements = root.elements
  
  def empty[A]: HashMap[K, A] = new HashMap(new EmptyNode[K])
}

object HashMap {
  def apply[K, V](pairs: (K, V)*) = pairs.foldLeft(new HashMap[K, V]) { _ + _ }
  
  def unapply[K, V](map: HashMap[K, V]) = map.toSeq
}

// ============================================================================
// nodes

private[collection] sealed trait Node[K, +V] {
  val size: Int
  
  def apply(key: K, hash: Int): Option[V]
  
  def update[A >: V](key: K, hash: Int, value: A): Node[K, A]
  
  def remove(key: K, hash: Int): Node[K, V]
  
  def elements: Iterator[(K, V)]
}


private[collection] class EmptyNode[K] extends Node[K, Nothing] {
  val size = 0
  
  def apply(key: K, hash: Int) = None
  
  def update[V](key: K, hash: Int, value: V) = new LeafNode(0, hash)(key, value)
  
  def remove(key: K, hash: Int) = this
  
  lazy val elements = new Iterator[(K, Nothing)] {
    val hasNext = false
    
    val next = null
  }
}


private[collection] class LeafNode[K, +V](shift: Int, hash: Int)(key: K, value: V) extends Node[K, V] {
  def apply(key: K, hash: Int) = if (this.key == key) Some(value) else None
  
  def update[A >: V](key: K, hash: Int, value: A) = {
    if (this.key == key) {
      new LeafNode(shift, hash)(key, value)
    } else if (this.hash == hash || hash == 0) {      // if we're bottoming out, just collide
      new CollisionNode(shift, hash, this.key -> this.value, key -> value)
    } else {
      BitmappedNode(shift)(Array((this.key, this.hash, this.value), (key, hash, value)))
    }
  }
  
  def remove(key: K, hash: Int) = if (this.key == key) new EmptyNode[K] else this
  
  def elements = new Iterator[(K, V)] {
    var hasNext = true
    
    def next = {
      hasNext = false
      (key, value)
    }
  }
  
  val size = 1
  
  override def toString = "LeafNode(" + key + " -> " + value + ")"
}


private[collection] class CollisionNode[K, +V](shift: Int, hash: Int, bucket: List[(K, V)]) extends Node[K, V] {
  def this(shift: Int, hash: Int, pairs: (K, V)*) = this(shift, hash, pairs.toList)
  
  def apply(key: K, hash: Int) = {
    for {
      (_, v) <- bucket find { case (k, _) => k == key }
    } yield v
  }
  
  def update[A >: V](key: K, hash: Int, value: A): Node[K, A] = {
    if (this.hash == hash) {
      var found = false
      
      val newBucket = for {
        (k, v) <- bucket
      } yield {
        if (k == key) {
          found = true
          (key, value)
        } else (k, v)
      }
      
      new CollisionNode(shift, hash, if (found) newBucket else (key, value) :: bucket)
    } else {
      val tempBucket = ((key, value) :: bucket).map({ case (k, v) => (k, k.hashCode, v) })
      BitmappedNode(shift)(tempBucket.toArray)   // not the most efficient, but not too bad
    }
  }
  
  def remove(key: K, hash: Int) = {
    if (bucket.exists({ case (k, _) => k == key }) && size == 2) {
      var pair: (K, V) = null
      
      for {
        (k, v) <- bucket
        if k == key
      } pair = (k, v)
      
      new LeafNode(shift, hash)(pair._1, pair._2)
    } else new CollisionNode(shift, hash, bucket.dropWhile({ case (k, _) => k == key }))
  }
  
  def elements = (bucket map { case (k, v) => (k, v) }).elements
  
  lazy val size = bucket.length
  
  override def toString = "CollisionNode(" + bucket.toString + ")"
}


private[collection] class BitmappedNode[K, +V](shift: Int)(table: Array[Node[K, V]], bits: Int, val size: Int) extends Node[K, V] {
  import BitmappedNode._
  
  def apply(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) table(i)(key, hash) else None
  }
  
  def update[A >: V](key: K, hash: Int, value: A): Node[K, A] = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    val newTable = new Array[Node[K, A]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    val newBits = addToTable(shift)(newTable, bits)(key, hash, value)
    val newSize = if (newBits == bits) size else size + 1
    
    if (newBits == Math.MIN_INT) {
      new FullNode(shift)(newTable, newSize)
    } else {
      new BitmappedNode(shift)(newTable, newBits, newSize)
    }
  }
  
  def remove(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) {
      val newTable = new Array[Node[K, V]](32)
      Array.copy(table, 0, newTable, 0, 32)
      
      val node = newTable(i).remove(key, hash)
      val newSize = size - (newTable(i).size - node.size)
      
      val newBits = if (node.isInstanceOf[EmptyNode[_]]) {
        newTable(i) = null
        bits ^ mask
      } else {
        newTable(i) = node
        bits
      }
      
      new BitmappedNode(shift)(newTable, newBits, newSize)
    } else this
  }
  
  def elements = {
    val iters = table flatMap { n => 
      if (n == null) Array[Iterator[(K, V)]]() else Array(n.elements)
    }
    
    iters.foldLeft(emptyElements) { _ ++ _ }
  }
  
  override def toString = "BitmappedNode(" + size + "," + table.filter(_ != null).toList.toString + ")"
  
  private lazy val emptyElements: Iterator[(K, V)] = new Iterator[(K, V)] {
    val hasNext = false
    
    val next = null
  }
}


private[collection] object BitmappedNode {
  def apply[K, V](shift: Int)(pairs: Array[(K, Int, V)]) = {
    val table = new Array[Node[K, V]](32)
    val bits = pairs.foldLeft(0) { (bits, pair) =>
      val (key, hash, value) = pair
      addToTable(shift)(table, bits)(key, hash, value)
    }
    
    new BitmappedNode(shift)(table, bits, pairs.length)
  }
  
  private def addToTable[K, V](shift: Int)(table: Array[Node[K, V]], bits: Int)(key: K, hash: Int, value: V) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) {
      table(i) = (table(i)(key, hash) = value)
    } else {
      table(i) = new LeafNode(shift + 5, hash)(key, value)
    }
    
    bits | mask
  }
}


private[collection] class FullNode[K, +V](shift: Int)(table: Array[Node[K, V]], val size: Int) extends Node[K, V] {
  def apply(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    
    table(i)(key, hash)
  }
  
  def update[A >: V](key: K, hash: Int, value: A) = {
    val i = (hash >>> shift) & 0x01f
    
    val newTable = new Array[Node[K, A]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    val node = newTable(i)(key, hash) = value
    val newSize = size + (node.size - newTable(i).size)
    newTable(i) = node
    
    new FullNode(shift)(newTable, newSize)
  }
  
  def remove(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    val newTable = new Array[Node[K, V]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    val node = newTable(i).remove(key, hash)
    val newSize = size - (newTable(i).size - node.size)
    
    if (node.isInstanceOf[EmptyNode[_]]) {
      newTable(i) = null
      new BitmappedNode(shift)(newTable, Math.MAX_INT ^ mask, newSize)
    } else {
      newTable(i) = node
      new FullNode(shift)(newTable, newSize)
    }
  }
  
  def elements = {
    val iters = table map { _.elements }
    iters.reduceLeft[Iterator[(K, V)]] { _ ++ _ }
  }
  
  override def toString = "FullNode"
}
