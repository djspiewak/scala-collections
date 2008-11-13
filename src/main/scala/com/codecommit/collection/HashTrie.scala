package com.codecommit.collection

/**
 * A clean-room port of Rich Hickey's persistent hash trie implementation from
 * Clojure (http://clojure.org).  Originally presented as a mutable structure in
 * a paper by Phil Bagwell.
 * 
 * @author Daniel Spiewak
 */
final class HashTrie[K, +V] private (root: Node[K, V]) extends Map[K, V] {
  lazy val size = root.size
  
  def this() = this(new EmptyNode[K])
  
  def get(key: K) = root(key, key.hashCode)
  
  override def +[A >: V](pair: (K, A)) = pair match {
    case (k, v) => update(k, v)
  }
  
  def update[A >: V](key: K, value: A) = new HashTrie(root(0, key, key.hashCode) = value)
  
  def -(key: K) = new HashTrie(root.remove(key, key.hashCode))
  
  def elements = root.elements
  
  def empty[A]: HashTrie[K, A] = new HashTrie(new EmptyNode[K])
  
  def diagnose = root.toString
}

object HashTrie {
  def apply[K, V](pairs: (K, V)*) = pairs.foldLeft(new HashTrie[K, V]) { _ + _ }
  
  def unapply[K, V](map: HashTrie[K, V]) = map.toSeq
}

// ============================================================================
// nodes

private[collection] sealed trait Node[K, +V] {
  val size: Int
  
  def apply(key: K, hash: Int): Option[V]
  
  def update[A >: V](shift: Int, key: K, hash: Int, value: A): Node[K, A]
  
  def remove(key: K, hash: Int): Node[K, V]
  
  def elements: Iterator[(K, V)]
}


private[collection] class EmptyNode[K] extends Node[K, Nothing] {
  val size = 0
  
  def apply(key: K, hash: Int) = None
  
  def update[V](shift: Int, key: K, hash: Int, value: V) = new LeafNode(key, hash, value)
  
  def remove(key: K, hash: Int) = this
  
  lazy val elements = new Iterator[(K, Nothing)] {
    val hasNext = false
    
    val next = null
  }
}


private[collection] class LeafNode[K, +V](key: K, hash: Int, value: V) extends Node[K, V] {
  val size = 1
  
  def apply(key: K, hash: Int) = if (this.key == key) Some(value) else None
  
  def update[A >: V](shift: Int, key: K, hash: Int, value: A) = {
    if (this.key == key) {
      if (this.value == value) this else new LeafNode(key, hash, value)
    } else if (this.hash == hash) {
      new CollisionNode(hash, this.key -> this.value, key -> value)
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
  
  override def toString = "LeafNode(" + key + " -> " + value + ")"
}


private[collection] class CollisionNode[K, +V](hash: Int, bucket: List[(K, V)]) extends Node[K, V] {
  lazy val size = bucket.length
  
  def this(hash: Int, pairs: (K, V)*) = this(hash, pairs.toList)
  
  def apply(key: K, hash: Int) = {
    for {
      (_, v) <- bucket find { case (k, _) => k == key }
    } yield v
  }
  
  def update[A >: V](shift: Int, key: K, hash: Int, value: A): Node[K, A] = {
    if (this.hash == hash) {
      var found = false
      
      val newBucket = for ((k, v) <- bucket) yield {
        if (k == key) {
          found = true
          (key, value)
        } else (k, v)
      }
      
      new CollisionNode(hash, if (found) newBucket else (key, value) :: bucket)
    } else {
      val tempBucket = ((key, value) :: bucket).map({ case (k, v) => (k, k.hashCode, v) })
      BitmappedNode(shift)(tempBucket.toArray)   // not the most efficient, but not too bad
    }
  }
  
  def remove(key: K, hash: Int) = {
    val newBucket = bucket filter { case (k, _) => k != key }
    
    if (newBucket.length == bucket.length) this else {
      if (newBucket.length == 1) {
        val (key, value) = newBucket.head
        new LeafNode(key, hash, value)
      } else new CollisionNode(hash, newBucket)
    }
  }
  
  def elements = bucket.elements
  
  override def toString = "CollisionNode(" + bucket.toString + ")"
}


private[collection] class BitmappedNode[K, +V](shift: Int)(table: Array[Node[K, V]], bits: Int) extends Node[K, V] {
  lazy val size = {
    val sizes = for {
      n <- table
      if n != null
    } yield n.size
    
    sizes.foldLeft(0) { _ + _ }
  }
  
  def apply(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) table(i)(key, hash) else None
  }
  
  def update[A >: V](levelShift: Int, key: K, hash: Int, value: A): Node[K, A] = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) {
      val node = (table(i)(shift + 5, key, hash) = value)
      
      if (node == table(i)) this else {
        val newTable = new Array[Node[K, A]](table.length)
        Array.copy(table, 0, newTable, 0, table.length)
        
        newTable(i) = node
        
        new BitmappedNode(shift)(newTable, bits)
      }
    } else {
      val newTable = new Array[Node[K, A]](Math.max(table.length, i + 1))
      Array.copy(table, 0, newTable, 0, table.length)
      
      newTable(i) = new LeafNode(key, hash, value)
      
      val newBits = bits | mask
      if (newBits == ~0) {
        new FullNode(shift)(newTable)
      } else {
        new BitmappedNode(shift)(newTable, newBits)
      }
    }
  }
  
  def remove(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    if ((bits & mask) == mask) {
      val node = table(i).remove(key, hash)
      
      if (node == table(i)) {
        this
      } else if (node.isInstanceOf[EmptyNode[_]]) {
        if (size == 1) new EmptyNode[K] else {
          val adjustedBits = bits ^ mask
          val log = Math.log(adjustedBits) / Math.log(2)
          
          if (log.toInt.toDouble == log) {      // last one
            table(log.toInt)
          } else {
            val newTable = new Array[Node[K, V]](if (i + 1 == table.length) table.length - 1 else table.length)
            Array.copy(table, 0, newTable, 0, newTable.length)
            
            newTable(i) = null
            
            new BitmappedNode(shift)(newTable, adjustedBits)
          }
        }
      } else {
        val newTable = new Array[Node[K, V]](table.length)
        Array.copy(table, 0, newTable, 0, table.length)
        
        newTable(i) = node
        
        new BitmappedNode(shift)(newTable, bits)
      }
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
    val table = new Array[Node[K, V]](pairs.foldLeft(0) { (x, pair) =>
      val (_, hash, _) = pair
      Math.max(x, (hash >>> shift) & 0x01f)
    } + 1)
    
    val bits = pairs.foldLeft(0) { (bits, pair) =>
      val (key, hash, value) = pair
      val i = (hash >>> shift) & 0x01f
      val mask = 1 << i
      
      if ((bits & mask) == mask) {
        table(i) = (table(i)(shift + 5, key, hash) = value)
      } else {
        table(i) = new LeafNode(key, hash, value)
      }
    
      bits | mask
    }
    
    new BitmappedNode(shift)(table, bits)
  }
}


private[collection] class FullNode[K, +V](shift: Int)(table: Array[Node[K, V]]) extends Node[K, V] {
  lazy val size = table.foldLeft(0) { _ + _.size }
  
  def apply(key: K, hash: Int) = table((hash >>> shift) & 0x01f)(key, hash)
  
  def update[A >: V](levelShift: Int, key: K, hash: Int, value: A) = {
    val i = (hash >>> shift) & 0x01f
    
    val node = (table(i)(shift + 5, key, hash) = value)
    
    if (node == table(i)) this else {
	    val newTable = new Array[Node[K, A]](32)
	    Array.copy(table, 0, newTable, 0, 32)
	    
	    newTable(i) = node
	    
	    new FullNode(shift)(newTable)
     }
  }
  
  def remove(key: K, hash: Int) = {
    val i = (hash >>> shift) & 0x01f
    val mask = 1 << i
    
    val node = table(i).remove(key, hash)
    
    if (node == table(i)) this else {
      val newTable = new Array[Node[K, V]](32)
      Array.copy(table, 0, newTable, 0, 32)
      
      if (node.isInstanceOf[EmptyNode[_]]) {
        newTable(i) = null
        new BitmappedNode(shift)(newTable, ~0 ^ mask)
      } else {
        newTable(i) = node
        new FullNode(shift)(newTable)
      }
    }
  }
  
  def elements = {
    val iters = table map { _.elements }
    iters.reduceLeft[Iterator[(K, V)]] { _ ++ _ }
  }
  
  override def toString = "FullNode(" + table.foldLeft("") { _.toString + ", " + _.toString } + ")"
}
