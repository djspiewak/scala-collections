package com.codecommit.collection

import scala.collection.immutable.TreeMap

import HashMap._

class HashMap[K, +V] private (root: Node[K, V]) extends Map[K, V] {
  def this() = this(new EmptyNode[K])
  
  def get(key: K) = root(key, key.hashCode)
  
  def update[A >: V](key: K, value: A) = new HashMap(root(key, key.hashCode) = value)
  
  def -(key: K) = new HashMap(root.remove(key, key.hashCode))
  
  def empty = new HashMap(new EmptyNode[K])
  
  def size = root.size
}

object HashMap {
  
}

// ===========================================================
// nodes

private[collection] sealed trait Node[K, +V] {
  def apply(key: K, hash: Int): Option[V]
  
  def update[A >: V](key: K, hash: Int, value: A): Node[K, A]
  
  def remove(key: K, hash: Int): Node[K, V]
  
  def size: Int
}

private[collection] class EmptyNode[K] extends Node[K, Nothing] {
  def apply(key: K, hash: Int) = None
  
  def update[A >: V](key: K, hash: Int, value: A) = new LeafNode(key value)
  
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
  
  val size = 1
}

private[collection] class CollisionNode[K, +V](tree: TreeMap[K, V]) extends Node[K, V] {
  
  def this(pairs: (K, V)*) = this(TreeMap(pairs:_*))
  
  def apply(key: K, hash: Int) = tree.get(key)
  
  def update[A >: V](key: K, hash: Int, value: A) = {
    val newTree = tree(key) = value
    new CollisionNode(newTree)
  }
  
  def remove(key: K, hash: Int) = {
    if (tree contains key && tree.size == 2) {
      var pair: (K, V) = _
      for ((k, v) <- tree; if k == key) pair = (k, v)
      
      new LeafNode(pair._1, pair._2)
    } else new CollisionNode(tree - key)
  }
  
  def size = tree.size
}

private[collection] class BitmappedNode[K, +V](table: Array[Node[K, V]], bits: Int, val size: Int) extends Node[K, V] {
  import BitmappedNode._
  
  def apply(key: K, hash: Int) = {
    val i = hash & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) table(i)(key, hash >> 5) else None
  }
  
  def update[A >: V](key: K, hash: Int, value: A) = {
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
}

private[collection] object BitmappedNode {
  def apply[K, V](pairs: (K, V)*) = {
    val table = new Array[Node[K, V]](32)
    val bits = pairs.foldLeft(0) { (bits, pair) =>
      val (key, value) = pair
      addToTable(table, bits)(key, key.hash, value)
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
    
    val newTable = new Array[Node[K, A]](32)
    Array.copy(table, 0, newTable, 0, 32)
    
    val node = newTable(i).remove(key, hash)
    val newSize = size - (newTable(i).size - node.size)
    
    if (node.isInstanceOf[EmptyNode]) {
      newTable(i) = null
      new BitmappedNode(newTable, Math.MAX_INT ^ mask, newSize)
    } else {
      newTable(i) = node
      new FullNode(newTable, newSize)
    }
  }
}
