package com.codecommit.collection

import Math.max
import PathVector._

/**
 * <p>An immutable implementation of the {@link Seq} interface with an array
 * backend.  Effectively, this class is an immutable vector.  The only wrinkle
 * in this design is the ability to store elements in <i>completely</i> arbitrary
 * indexes (to enable use as a random-access array).  Thus, the length of this
 * data structure is effectively infinite.  The {@link #length} field is defined
 * to return the maximum index of the elements in the vector.</p>
 * 
 * <p>The underlying data structure for the persistent vector is a trie with an
 * extremely high branching factor (by default: 32).  Each trie node contains
 * an array representing each branch.  This implementation allows almost-constant
 * time access coupled with minimal data-copying on insert.  The ratio between
 * these two is controlled by the branching factor.  A higher branching factor
 * will lead to a better worst-case access time (asymtotically more constant)
 * while a lower branching factor will result in asymtotically less data
 * copying on insert.</p>
 *
 * <p>As is natural for an immutable data structure, this vector is parameterized
 * covariantly.  This poses a few minor difficulties in implementation, due to
 * the fact that arrays, as mutable containers, are parameterized
 * <i>invariantly</i>.  To get around this, some up-casting is utilized durring
 * insertion.  This is considered to be sound due to the fact that the type
 * system will ensure that the casting is always upwards, rather than down (which
 * is where the mutability concerns come into play).</p>
 * 
 * <p>The underlying determinant for the trie structure in this implementation is
 * the big-endian component value of the index in question, converted into a path
 * to the final trie node.  This still yields a maximum depth of 7 (when dealing
 * with 32-bit integer indexes and a branching factor of 32), but the average
 * depth of the trie is higher and its width is proportionately lower.  This
 * yields far more efficient write operations than a bit-partitioned trie, as well
 * as a tremendously reduced memory footprint.  Additionally, this implementation
 * allows a configurable branching factor, whereas the branching factor of a
 * bit-partitioned implemnetation must always be a factor of 2.</p>
 * 
 * @author Daniel Spiewak
 */
class PathVector[+T] private (private val data: Option[T], val length: Int, val branchingFactor: Int,
          private val left: Seq[PathVector[T]], private val right: Seq[PathVector[T]]) extends RandomAccessSeq[T] { outer =>
  
  def this(branchingFactor: Int) = this(None, 0, branchingFactor, EmptyArray, EmptyArray)
  
  def this() = this(32)
  
  def apply(i: Int) = getOrElse(i, null.asInstanceOf[T])
  
  def get(i: Int) = locate(computePath(i, branchingFactor))
  
  def getOrElse[A >: T](i: Int, default: A) = get(i) match {
    case Some(x) => x
    case None => default
  }
  
  private def locate(path: List[Int]): Option[T] = path match {
    case hd :: tail => {
      val branches = if (hd < (branchingFactor / 2)) left else right
      val node = if (hd < branches.length) branches(hd) else null.asInstanceOf[PathVector[T]]
      
      if (node == null) None else node.locate(tail)
    }
    
    case Nil => data
  }
  
  // somewhat less efficient than it could be
  override def ++[A >: T](other: Iterable[A]) = other.foldLeft(this:PathVector[A]) { _ + _ }
  
  def +[A >: T](e: A) = update(length, e)
  
  def update[A >: T](i: Int, e: A) = store(computePath(i, branchingFactor), e)
  
  private def store[A >: T](path: List[Int], e: A): PathVector[A] = path match {
    case hd :: tail => {
      val branches = if (hd < (branchingFactor / 2)) left else right
      val node = if (hd < branches.length) branches(hd) else null.asInstanceOf[PathVector[T]]
      val vector = if (node == null) EmptyPathVector(branchingFactor) else node
      
      val newBranches = new Array[PathVector[A]](max(branches.length, hd + 1))
      Array.copy(branches, 0, newBranches, 0, branches.length)
      
      newBranches(hd) = vector.store(tail, e)
      
      val newLeft = if (hd < (branchingFactor / 2)) newBranches else left
      val newRight = if (hd < (branchingFactor / 2)) right else newBranches
      
      new PathVector(data, max(length, flattenPath(path, branchingFactor) + 1), branchingFactor, newLeft, newRight)
    }
    
    case Nil => new PathVector(Some(e), max(length, flattenPath(path, branchingFactor) + 1), branchingFactor, left, right)
  }
  
  override def filter(p: (T)=>Boolean) = {
    var back = new PathVector[T]
    var i = 0
    
    while (i < length) {
      val e = apply(i)
      if (p(e)) back += e
      
      i += 1
    }
    
    back
  }
  
  override def flatMap[A](f: (T)=>Iterable[A]) = {
    var back = new PathVector[A]
    var i = 0
    
    while (i < length) {
      f(apply(i)) foreach { back += _ }
      i += 1
    }
    
    back
  }
  
  override def map[A](f: (T)=>A): PathVector[A] = {
    var back = new PathVector[A]
    var i = 0
    
    while (i < length) {
      back += f(apply(i))
      i += 1
    }
    
    back
  }
  
  override def reverse = new PathVector[T](branchingFactor) {
    override val length = outer.length
    
    override def get(i: Int) = outer.get(length - i - 1)
    
    override def update[A >: T](i: Int, e: A) = {
      var back = new PathVector[A]
      
      foreachOption { (c, x) =>
        back = back(c) = if (c == i) e else x
      }
      
      if (i < 0) {
        throw new IndexOutOfBoundsException(i.toString)
      } else if (i >= length) {
        back(i) = e
      } else {
        back
      }
    }
  }
  
  override def subseq(from: Int, end: Int) = subPathVector(from, end)
  
  def subPathVector(from: Int, end: Int) = {
    if (from < 0) {
      throw new IndexOutOfBoundsException(from.toString)
    } else if (end <= from) {
      throw new IllegalArgumentException("Invalid range: " + from + ".." + end)
    } else {
      new PathVector[T](branchingFactor) {
        override val length = end - from
        
        override def get(i: Int) = outer.get(i + from)
        
        override def update[A >: T](i: Int, e: A) = {
          var back = new PathVector[A]
          
          foreachOption { (c, x) =>
            back = back(c) = if (c == i) e else x
          }
          
          if (i < 0) {
            throw new IndexOutOfBoundsException(i.toString)
          } else if (i >= length) {
            back(i) = e
          } else {
            back
          }
        }
      }
    }
  }
  
  def zip[A](that: PathVector[A]) = {
    var back = new PathVector[(T, A)]
    var i = 0
    
    val limit = max(length, that.length)
    while (i < limit) {
      back += (apply(i), that(i))
      i += 1
    }
    
    back
  }
  
  def zipWithIndex = {
    var back = new PathVector[(T, Int)]
    var i = 0
    
    while (i < length) {
      back += (apply(i), i)
      i += 1
    }
    
    back
  }
  
  override def equals(other: Any) = other match {
    case vec:PathVector[T] => {
      var back = length == vec.length
      var i = 0
      
      while (i < length) {
        back &&= get(i) == vec.get(i)
        i += 1
      }
      
      back
    }
    
    case _ => false
  }
  
  @inline
  private def foreachOption(f: (Int, T)=>Unit) = {
    var i = 0
    while (i < length) {
      get(i) match {
        case Some(x) => f(i, x)
        case None => ()
      }
      
      i += 1
    }
  }
}

object PathVector {
  private[collection] val EmptyArray = new Array[PathVector[Nothing]](0)
  
  def apply[T](elems: T*) = {
    var vector = new PathVector[T]
    var i = 0
    
    for (e <- elems) {
      vector = vector(i) = e
      i += 1
    }
    vector
  }
  
  @inline
  private[collection] def computePath(total: Int, base: Int) = {
    if (total < 0) {
      throw new IndexOutOfBoundsException(total.toString)
    } else {
      var back: List[Int] = Nil
      var num = total
      
      do {
        back = (num % base) :: back
        num /= base
      } while (num > 0)
      
      back
    }
  }
  
  @inline
  private[collection] def flattenPath(path: List[Int], base: Int) = path.foldLeft(0) { _ * base + _ }
}

object EmptyPathVector {
  private var cache = Map[Int, PathVector[Nothing]]()
  
  def apply(branchingFactor: Int) = {
    if (cache contains branchingFactor) cache(branchingFactor) else {
      val back = new PathVector[Nothing](branchingFactor)
      cache += (branchingFactor -> back)
      
      back
    }
  }
}
