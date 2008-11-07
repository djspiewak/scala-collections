package com.codecommit.collection

import HashMap._

class HashMap[K, +V] private (root: Node[K, V]) extends Map[K, V] {
  def this() = this(new EmptyNode[K])
  
  def get(key: K) = root(key, key.hashCode)
  
  def update[A >: V](key: K, value: A) = new HashMap(root(key, key.hashCode) = value)
  
  def -(key: K) = new HashMap(root.remove(key, key.hashCode))
  
  def elements = root.elements
  
  def empty[A]: HashMap[K, A] = new HashMap(new EmptyNode[K])
  
  lazy val size = root.size
}

object HashMap {
  
}

// ===========================================================
// nodes

private[collection] sealed trait Node[K, +V] {
  def apply(key: K, hash: Int): Option[V]
  
  def update[A >: V](key: K, hash: Int, value: A): Node[K, A]
  
  def remove(key: K, hash: Int): Node[K, V]
  
  def elements: Iterator[(K, V)]
  
  val size: Int
}

private[collection] class EmptyNode[K] extends Node[K, Nothing] {
  def apply(key: K, hash: Int) = None
  
  def update[V](key: K, hash: Int, value: V) = new LeafNode(key, value)
  
  def remove(key: K, hash: Int) = this
  
  lazy val elements = new Iterator[(K, Nothing)] {
    val hasNext = false
    
    val next = null
  }
  
  val size = 0
}

private[collection] class LeafNode[K, +V](key: K, value: V) extends Node[K, V] {
  def apply(key: K, hash: Int) = if (this.key == key) Some(value) else None
  
  def update[A >: V](key: K, hash: Int, value: A) = {
    if (this.key == key) {
      new LeafNode(key, value)
    } else if (this.key.hashCode == key.hashCode) {
      new CollisionNode(this.key -> this.value, key -> value)
    } else {
      BitmappedNode(this.key -> this.value, key -> value)
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
}

private[collection] class CollisionNode[K, +V](bucket: List[(K, V)]) extends Node[K, V] {
  
  def this(pairs: (K, V)*) = this(pairs.toList)
  
  def apply(key: K, hash: Int) = for {
    (k, v) <- bucket find { case (k, _) => k == key }
  } yield v
  
  def update[A >: V](key: K, hash: Int, value: A) = {
    var found = false
    
    val newBucket = for {
      (k, v) <- bucket
    } yield {
      if (k == key) {
        found = true
        (key, value)
      } else (k, v)
    }
    
    new CollisionNode(if (found) newBucket else (key, value) :: bucket)
  }
  
  def remove(key: K, hash: Int) = {
    if (bucket.exists({ case (k, v) => k == key }) && size == 2) {
      var pair: (K, V) = null
      
      for {
        (k, v) <- bucket
        if k == key
      } pair = (k, v)
      
      new LeafNode(pair._1, pair._2)
    } else new CollisionNode(bucket.dropWhile({ case (k, v) => k == key }))
  }
  
  def elements = bucket.elements
  
  lazy val size = bucket.length
}

private[collection] class BitmappedNode[K, +V](table: Array[Node[K, V]], bits: Int, val size: Int) extends Node[K, V] {
  import BitmappedNode._
  
  def apply(key: K, hash: Int) = {
    val i = hash & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) table(i)(key, hash >> 5) else None
  }
  
  def update[A >: V](key: K, hash: Int, value: A): Node[K, A] = {
    val i = hash & 0x01f
    val mask = 1 << i
    
    val newTable = new Array[Node[K, A]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    val newBits = addToTable(newTable, bits)(key, hash, value)
    val newSize = if (newBits == bits) size else size + 1
    
    if (newBits == Math.MIN_INT) {
      new FullNode(newTable, newSize)
    } else {
      new BitmappedNode(newTable, newBits, newSize)
    }
  }
  
  def remove(key: K, hash: Int) = {
    val i = hash & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) {
      val newTable = new Array[Node[K, V]](32)
      Array.copy(table, 0, newTable, 0, 32)
      
      val node = newTable(i).remove(key, hash >> 5)
      val newSize = size - (newTable(i).size - node.size)
      
      val newBits = if (node.isInstanceOf[EmptyNode[_]]) {
        newTable(i) = null
        bits ^ mask
      } else {
        newTable(i) = node
        bits
      }
      
      new BitmappedNode(newTable, newBits, newSize)
    } else this
  }
  
  def elements = {
    val iters = table flatMap { n => 
      if (n == null) Array[Iterator[(K, V)]]() else Array(n.elements)
    }
    
    iters.foldLeft(emptyElements) { _ ++ _ }
  }
  
  private lazy val emptyElements: Iterator[(K, V)] = new Iterator[(K, V)] {
    val hasNext = false
    
    val next = null
  }
}

private[collection] object BitmappedNode {
  def apply[K, V](pairs: (K, V)*) = {
    val table = new Array[Node[K, V]](32)
    val bits = pairs.foldLeft(0) { (bits, pair) =>
      val (key, value) = pair
      addToTable(table, bits)(key, key.hashCode, value)
    }
    
    new BitmappedNode(table, bits, pairs.length)
  }
  
  private def addToTable[K, V](table: Array[Node[K, V]], bits: Int)(key: K, hash: Int, value: V) = {
    val i = hash & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) {
      table(i) = (table(i)(key, hash >> 5) = value)
      
      bits
    } else {
      val newBits = bits | mask
      table(i) = new LeafNode(key, value)
      
      newBits
    }
  }
}

private[collection] class FullNode[K, +V](table: Array[Node[K, V]], val size: Int) extends Node[K, V] {
  def apply(key: K, hash: Int) = {
    val i = hash & 0x01f
    
    table(i)(key, hash >> 5)
  }
  
  def update[A >: V](key: K, hash: Int, value: A) = {
    val i = hash & 0x01f
    
    val newTable = new Array[Node[K, A]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    newTable(i) = newTable(i)(key, hash >> 5) = value
    new FullNode(newTable, size)
  }
  
  def remove(key: K, hash: Int) = {
    val i = hash & 0x01f
    val mask = 1 << i
    
    val newTable = new Array[Node[K, V]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    val node = newTable(i).remove(key, hash)
    val newSize = size - (newTable(i).size - node.size)
    
    if (node.isInstanceOf[EmptyNode[_]]) {
      newTable(i) = null
      new BitmappedNode(newTable, Math.MAX_INT ^ mask, newSize)
    } else {
      newTable(i) = node
      new FullNode(newTable, newSize)
    }
  }
  
  def elements = {
    val iters = table map { _.elements }
    iters.reduceLeft[Iterator[(K, V)]] { _ ++ _ }
  }
}
