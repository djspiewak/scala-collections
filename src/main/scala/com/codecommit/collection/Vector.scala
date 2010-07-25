/**
 Copyright (c) 2007-2008, Rich Hickey
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

 * Neither the name of Clojure nor the names of its contributors
   may be used to endorse or promote products derived from this
   software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
 **/

package com.codecommit.collection

import Vector._
import VectorCases._

/**
 * A straight port of Clojure's <code>PersistentVector</code> class.
 *
 * @author Daniel Spiewak
 * @author Rich Hickey
 */
class Vector[+T] private (val length: Int, trie: Case, tail: Array[AnyRef]) extends RandomAccessSeq[T] { outer =>
  private val tailOff = length - tail.length
  
  /*
   * The design of this data structure inherantly requires heterogenous arrays.
   * It is *possible* to design around this, but the result is comparatively
   * quite inefficient.  With respect to this fact, I have left the original
   * (somewhat dynamically-typed) implementation in place.
   */
  
  private[collection] def this() = this(0, Zero, EmptyArray)
  
  def apply(i: Int): T = {
    if (i >= 0 && i < length) {
      if (i >= tailOff) {
        tail(i & 0x01f).asInstanceOf[T]
      } else {
        var arr = trie(i)
        arr(i & 0x01f).asInstanceOf[T]
      }
    } else throw new IndexOutOfBoundsException(i.toString)
  }
  
  def update[A >: T](i: Int, obj: A): Vector[A] = {
    if (i >= 0 && i < length) {
      if (i >= tailOff) {
        val newTail = new Array[AnyRef](tail.length)
        Array.copy(tail, 0, newTail, 0, tail.length)
        newTail(i & 0x01f) = obj.asInstanceOf[AnyRef]
        
        new Vector[A](length, trie, newTail)
      } else {
        new Vector[A](length, trie(i) = obj.asInstanceOf[AnyRef], tail)
      }
    } else if (i == length) {
      this + obj
    } else throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def ++[A >: T](other: Iterable[A]) = other.foldLeft(this: Vector[A]) { _ + _ }
  
  def +[A >: T](obj: A): Vector[A] = {
    if (tail.length < 32) {
      val tail2 = new Array[AnyRef](tail.length + 1)
      Array.copy(tail, 0, tail2, 0, tail.length)
      tail2(tail.length) = obj.asInstanceOf[AnyRef]
      
      new Vector[A](length + 1, trie, tail2)
    } else {
      new Vector[A](length + 1, trie + tail, array(obj.asInstanceOf[AnyRef]))
    }
  }
  
  /**
   * Removes the <i>tail</i> element of this vector.
   */
  def pop: Vector[T] = {
    if (length == 0) {
      throw new IllegalStateException("Can't pop empty vector")
    } else if (length == 1) {
      EmptyVector
    } else if (tail.length > 1) {
      val tail2 = new Array[AnyRef](tail.length - 1)
      Array.copy(tail, 0, tail2, 0, tail2.length)
      
      new Vector[T](length - 1, trie, tail2)
    } else {
      val (trie2, tail2) = trie.pop
      new Vector[T](length - 1, trie2, tail2)
    }
  }
  
  override def filter(p: (T)=>Boolean) = {
    var back = new Vector[T]
    var i = 0
    
    while (i < length) {
      val e = apply(i)
      if (p(e)) back += e
      
      i += 1
    }
    
    back
  }
  
  override def flatMap[A](f: (T)=>Iterable[A]) = {
    var back = new Vector[A]
    var i = 0
    
    while (i < length) {
      f(apply(i)) foreach { back += _ }
      i += 1
    }
    
    back
  }
  
  override def map[A](f: (T)=>A) = {
    var back = new Vector[A]
    var i = 0
    
    while (i < length) {
      back += f(apply(i))
      i += 1
    }
    
    back
  }
  
  override def reverse: Vector[T] = new VectorProjection[T] {
    override val length = outer.length
    
    override def apply(i: Int) = outer.apply(length - i - 1)
  }
  
  override def subseq(from: Int, end: Int) = subVector(from, end)
  
  def subVector(from: Int, end: Int): Vector[T] = {
    if (from < 0) {
      throw new IndexOutOfBoundsException(from.toString)
    } else if (end >= length) {
      throw new IndexOutOfBoundsException(end.toString)
    } else if (end <= from) {
      throw new IllegalArgumentException("Invalid range: " + from + ".." + end)
    } else {
      new VectorProjection[T] {
        override val length = end - from
        
        override def apply(i: Int) = outer.apply(i + from)
      }
    }
  }
  
  def zip[A](that: Vector[A]) = {
    var back = new Vector[(T, A)]
    var i = 0
    
    val limit = Math.min(length, that.length)
    while (i < limit) {
      back += (apply(i), that(i))
      i += 1
    }
    
    back
  }
  
  def zipWithIndex = {
    var back = new Vector[(T, Int)]
    var i = 0
    
    while (i < length) {
      back += (apply(i), i)
      i += 1
    }
    
    back
  }
  
  override def equals(other: Any) = other match {
    case vec:Vector[T] => {
      var back = length == vec.length
      var i = 0
      
      while (i < length) {
        back &&= apply(i) == vec.apply(i)
        i += 1
      }
      
      back
    }
    
    case _ => false
  }
  
  override def hashCode = foldLeft(0) { _ ^ _.hashCode }
}

object Vector {
  private[collection] val EmptyArray = new Array[AnyRef](0)
  
  def apply[T](elems: T*) = elems.foldLeft(EmptyVector:Vector[T]) { _ + _ }
  
  def unapplySeq[T](vec: Vector[T]): Option[Seq[T]] = Some(vec)
  
  @inline
  private[collection] def array(elem: AnyRef) = {
    val back = new Array[AnyRef](1)
    back(0) = elem
    back
  }
}

object EmptyVector extends Vector[Nothing]

private[collection] abstract class VectorProjection[+T] extends Vector[T] {
  override val length: Int
  override def apply(i: Int): T
  
  override def +[A >: T](e: A) = innerCopy + e
  
  override def update[A >: T](i: Int, e: A) = {
    if (i < 0) {
      throw new IndexOutOfBoundsException(i.toString)
    } else if (i > length) {
      throw new IndexOutOfBoundsException(i.toString)
    } else innerCopy(i) = e
  }
  
  private lazy val innerCopy = foldLeft(EmptyVector:Vector[T]) { _ + _ }
}

private[collection] object VectorCases {
  private val SingletonArray1 = new Array[Array[AnyRef]](1)
  private val SingletonArray2 = new Array[Array[Array[AnyRef]]](1)
  private val SingletonArray3 = new Array[Array[Array[Array[AnyRef]]]](1)
  private val SingletonArray4 = new Array[Array[Array[Array[Array[AnyRef]]]]](1)
  
  private def copy[A](array: Array[A], length: Int): Array[A] = {
    val array2 = new Array[A](length)
    Array.copy(array, 0, array2, 0, Math.min(array.length, length))
    array2
  }
  
  private def copy[A](array: Array[A]): Array[A] = copy(array, array.length)
  
  sealed trait Case {
    type Self <: Case
    
    val shift: Int
    
    def apply(i: Int): Array[AnyRef]
    def update(i: Int, obj: AnyRef): Self
    
    def +(node: Array[AnyRef]): Case
    def pop: (Case, Array[AnyRef])
  }
  
  case object Zero extends Case {
    type Self = Nothing
    
    val shift = -1
    
    def apply(i: Int) = throw new IndexOutOfBoundsException(i.toString)
    def update(i: Int, obj: AnyRef) = throw new IndexOutOfBoundsException(i.toString)
    
    def +(node: Array[AnyRef]) = One(node)
    def pop = throw new IndexOutOfBoundsException("Cannot pop an empty Vector")
  }
  
  case class One(trie: Array[AnyRef]) extends Case {
    type Self = One
    
    val shift = 0
    
    def apply(i: Int) = trie
    
    def update(i: Int, obj: AnyRef) = {
      val trie2 = copy(trie)
      trie2(i & 0x01f) = obj
      One(trie2)
    }
    
    def +(tail: Array[AnyRef]) = {
      val trie2 = new Array[Array[AnyRef]](2)
      trie2(0) = trie
      trie2(1) = tail
      Two(trie2)
    }
    
    def pop = (Zero, trie)
  }
  
  case class Two(trie: Array[Array[AnyRef]]) extends Case {
    type Self = Two
    
    val shift = 5
    
    def apply(i: Int) = trie((i >>> 5) & 0x01f)
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy(trie)
      
      val trie2b = copy(trie2a((i >>> 5) & 0x01f))
      trie2a((i >>> 5) & 0x01f) = trie2b
      
      trie2b(i & 0x01f) = obj
      Two(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.length >= 32) {
        val trie2 = new Array[Array[Array[AnyRef]]](2)
        trie2(0) = trie
        
        trie2(1) = SingletonArray1
        trie2(1)(0) = tail
        
        Three(trie2)
      } else {
        val trie2 = copy(trie, trie.length + 1)
        trie2(trie.length) = tail
        Two(trie2)
      }
    }
    
    def pop = {
      if (trie.length == 2) {
        (One(trie(0)), trie.last)
      } else {
        val trie2 = copy(trie, trie.length - 1)
        (Two(trie2), trie.last)
      }
    }
  }
  
  case class Three(trie: Array[Array[Array[AnyRef]]]) extends Case {
    type Self = Three
    
    val shift = 10
    
    def apply(i: Int) = {
      val a = trie((i >>> 10) & 0x01f)
      a((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy(trie)
      
      val trie2b = copy(trie2a((i >>> 10) & 0x01f))
      trie2a((i >>> 10) & 0x01f) = trie2b
      
      val trie2c = copy(trie2b((i >>> 5) & 0x01f))
      trie2b((i >>> 5) & 0x01f) = trie2c
      
      trie2c(i & 0x01f) = obj
      Three(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.length >= 32) {
        if (trie.length >= 32) {
          val trie2 = new Array[Array[Array[Array[AnyRef]]]](2)
          trie2(0) = trie
          
          trie2(1) = SingletonArray2
          trie2(1)(0) = SingletonArray1
          trie2(1)(0)(0) = tail
          
          Four(trie2)
        } else {
          val trie2 = copy(trie, trie.length + 1)
          trie2(trie.length) = SingletonArray1
          trie2(trie.length)(0) = tail
          Three(trie2)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length + 1)
        trie2.last(trie.last.length) = tail
        Three(trie2)
      }
    }
    
    def pop = {
      if (trie.last.length == 1) {
        if (trie.length == 2) {
          (Two(trie(0)), trie.last.last)
        } else {
          val trie2 = copy(trie, trie.length - 1)
          (Three(trie2), trie.last.last)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
        (Three(trie2), trie.last.last)
      }
    }
  }
  
  case class Four(trie: Array[Array[Array[Array[AnyRef]]]]) extends Case {
    type Self = Four
    
    val shift = 15
    
    def apply(i: Int) = {
      val a = trie((i >>> 15) & 0x01f)
      val b = a((i >>> 10) & 0x01f)
      b((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy(trie)
      
      val trie2b = copy(trie2a((i >>> 15) & 0x01f))
      trie2a((i >>> 15) & 0x01f) = trie2b
      
      val trie2c = copy(trie2b((i >>> 10) & 0x01f))
      trie2b((i >>> 10) & 0x01f) = trie2c
      
      val trie2d = copy(trie2c((i >>> 5) & 0x01f))
      trie2c((i >>> 5) & 0x01f) = trie2d
      
      trie2d(i & 0x01f) = obj
      Four(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.length >= 32) {
        if (trie.last.length >= 32) {
          if (trie.length >= 32) {
            val trie2 = new Array[Array[Array[Array[Array[AnyRef]]]]](2)
            trie2(0) = trie
            
            trie2(1) = SingletonArray3
            trie2(1)(0) = SingletonArray2
            trie2(1)(0)(0) = SingletonArray1
            trie2(1)(0)(0)(0) = tail
            
            Five(trie2)
          } else {
            val trie2 = copy(trie, trie.length + 1)
            trie2(trie.length) = SingletonArray2
            trie2(trie.length)(0) = SingletonArray1
            trie2(trie.length)(0)(0) = tail
            Four(trie2)
          }
        } else {
          val trie2 = copy(trie)
          trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length + 1)
          trie2.last(trie.last.length) = SingletonArray1
          trie2.last.last(0) = tail
          Four(trie2)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last)
        trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length + 1)
        trie2.last.last(trie.last.last.length) = tail
        Four(trie2)
      }
    }
    
    def pop = {
      if (trie.last.last.length == 1) {
        if (trie.last.length == 1) {
          if (trie.length == 2) {
            (Three(trie(0)), trie.last.last.last)
          } else {
            val trie2 = copy(trie, trie.length - 1)
            (Four(trie2), trie.last.last.last)
          }
        } else {
          val trie2 = copy(trie)
          trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
          (Four(trie2), trie.last.last.last)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
        trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length - 1)
        (Four(trie2), trie.last.last.last)
      }
    }
  }
  
  case class Five(trie: Array[Array[Array[Array[Array[AnyRef]]]]]) extends Case {
    type Self = Five
    
    val shift = 20
    
    def apply(i: Int) = {
      val a = trie((i >>> 20) & 0x01f)
      val b = a((i >>> 15) & 0x01f)
      val c = b((i >>> 10) & 0x01f)
      c((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy(trie)
      
      val trie2b = copy(trie2a((i >>> 20) & 0x01f))
      trie2a((i >>> 20) & 0x01f) = trie2b
      
      val trie2c = copy(trie2b((i >>> 15) & 0x01f))
      trie2b((i >>> 15) & 0x01f) = trie2c
      
      val trie2d = copy(trie2c((i >>> 10) & 0x01f))
      trie2c((i >>> 10) & 0x01f) = trie2d
      
      val trie2e = copy(trie2d((i >>> 5) & 0x01f))
      trie2d((i >>> 5) & 0x01f) = trie2e
      
      trie2e(i & 0x01f) = obj
      Five(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.last.length >= 32) {
        if (trie.last.last.length >= 32) {
          if (trie.last.length >= 32) {
            if (trie.length >= 32) {
              val trie2 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](2)
              trie2(0) = trie
              
              trie2(1) = SingletonArray4
              trie2(1)(0) = SingletonArray3
              trie2(1)(0)(0) = SingletonArray2
              trie2(1)(0)(0)(0) = SingletonArray1
              trie2(1)(0)(0)(0)(0) = tail
              
              Six(trie2)
            } else {
              val trie2 = copy(trie, trie.length + 1)
              trie2(trie.length) = SingletonArray3
              trie2(trie.length)(0) = SingletonArray2
              trie2(trie.length)(0)(0) = SingletonArray1
              trie2(trie.length)(0)(0)(0) = tail
              Five(trie2)
            }
          } else {
            val trie2 = copy(trie)
            trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length + 1)
            trie2.last(trie.last.length) = SingletonArray2
            trie2.last.last(0) = SingletonArray1
            trie2.last.last.last(0) = tail
            Five(trie2)
          }
        } else {
          val trie2 = copy(trie)
          trie2(trie2.length - 1) = copy(trie2.last)
          trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length + 1)
          trie2.last.last(trie.last.last.length) = SingletonArray1
          trie2.last.last.last(0) = tail
          Five(trie2)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last)
        trie2.last(trie2.last.length - 1) = copy(trie2.last.last)
        trie2.last.last(trie2.last.last.length - 1) = copy(trie2.last.last.last, trie2.last.last.last.length + 1)
        trie2.last.last.last(trie.last.last.last.length) = tail
        Five(trie2)
      }
    }
    
    def pop = {
      if (trie.last.last.last.length == 1) {
        if (trie.last.last.length == 1) {
          if (trie.last.length == 1) {
            if (trie.length == 2) {
              (Four(trie(0)), trie.last.last.last.last)
            } else {
              val trie2 = copy(trie, trie.length - 1)
              (Five(trie2), trie.last.last.last.last)
            }
          } else {
            val trie2 = copy(trie)
            trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
            (Five(trie2), trie.last.last.last.last)
          }
        } else {
          val trie2 = copy(trie)
          trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
          trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length - 1)
          (Five(trie2), trie.last.last.last.last)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
        trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length - 1)
        trie2.last.last(trie2.last.last.length - 1) = copy(trie2.last.last.last, trie2.last.last.last.length - 1)
        (Five(trie2), trie.last.last.last.last)
      }
    }
  }
  
  case class Six(trie: Array[Array[Array[Array[Array[Array[AnyRef]]]]]]) extends Case {
    type Self = Six
    
    val shift = 25
    
    def apply(i: Int) = {
      val a = trie((i >>> 25) & 0x01f)
      val b = a((i >>> 20) & 0x01f)
      val c = b((i >>> 15) & 0x01f)
      val d = c((i >>> 10) & 0x01f)
      d((i >>> 5) & 0x01f)
    }
    
    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy(trie)
      
      val trie2b = copy(trie2a((i >>> 25) & 0x01f))
      trie2a((i >>> 25) & 0x01f) = trie2b
      
      val trie2c = copy(trie2b((i >>> 20) & 0x01f))
      trie2b((i >>> 20) & 0x01f) = trie2c
      
      val trie2d = copy(trie2c((i >>> 15) & 0x01f))
      trie2c((i >>> 15) & 0x01f) = trie2d
      
      val trie2e = copy(trie2d((i >>> 10) & 0x01f))
      trie2d((i >>> 10) & 0x01f) = trie2e
      
      val trie2f = copy(trie2e((i >>> 5) & 0x01f))
      trie2e((i >>> 5) & 0x01f) = trie2f
      
      trie2f(i & 0x01f) = obj
      Six(trie2a)
    }
    
    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.last.last.length >= 32) {
        if (trie.last.last.last.length >= 32) {
          if (trie.last.last.length >= 32) {
            if (trie.last.length >= 32) {
              if (trie.length >= 32) {
                throw new IndexOutOfBoundsException("Cannot grow vector beyond integer bounds")
              } else {
                val trie2 = copy(trie, trie.length + 1)
                trie2(trie.length) = SingletonArray4
                trie2(trie.length)(0) = SingletonArray3
                trie2(trie.length)(0)(0) = SingletonArray2
                trie2(trie.length)(0)(0)(0) = SingletonArray1
                trie2(trie.length)(0)(0)(0)(0) = tail
                Six(trie2)
              }
            } else {
              val trie2 = copy(trie)
              trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length + 1)
              trie2.last(trie.last.length) = SingletonArray3
              trie2.last.last(0) = SingletonArray2
              trie2.last.last.last(0) = SingletonArray1
              trie2.last.last.last.last(0) = tail
              Six(trie2)
            }
          } else {
            val trie2 = copy(trie)
            trie2(trie2.length - 1) = copy(trie2.last)
            trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length + 1)
            trie2.last.last(trie.last.last.length) = SingletonArray2
            trie2.last.last.last(0) = SingletonArray1
            trie2.last.last.last.last(0) = tail
            Six(trie2)
          }
        } else {
          val trie2 = copy(trie)
          trie2(trie2.length - 1) = copy(trie2.last)
          trie2.last(trie2.last.length - 1) = copy(trie2.last.last)
          trie2.last.last(trie2.last.last.length - 1) = copy(trie2.last.last.last, trie2.last.last.last.length + 1)
          trie2.last.last.last(trie.last.last.last.length) = SingletonArray1
          trie2.last.last.last.last(0) = tail
          Six(trie2)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last)
        trie2.last(trie2.last.length - 1) = copy(trie2.last.last)
        trie2.last.last(trie2.last.last.length - 1) = copy(trie2.last.last.last)
        trie2.last.last.last(trie.last.last.last.length - 1) = copy(trie2.last.last.last.last, trie2.last.last.last.last.length + 1)
        trie2.last.last.last.last(trie.last.last.last.last.length) = tail
        Six(trie2)
      }
    }
    
    def pop = {
      if (trie.last.last.last.last.length == 1) {
        if (trie.last.last.last.length == 1) {
          if (trie.last.last.length == 1) {
            if (trie.last.length == 1) {
              if (trie.length == 2) {
                (Five(trie(0)), trie.last.last.last.last.last)
              } else {
                val trie2 = copy(trie, trie.length - 1)
                (Six(trie2), trie.last.last.last.last.last)
              }
            } else {
              val trie2 = copy(trie)
              trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
              (Six(trie2), trie.last.last.last.last.last)
            }
          } else {
            val trie2 = copy(trie)
            trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
            trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length - 1)
            (Six(trie2), trie.last.last.last.last.last)
          }
        } else {
          val trie2 = copy(trie)
          trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
          trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length - 1)
          trie2.last.last(trie2.last.last.length - 1) = copy(trie2.last.last.last, trie2.last.last.last.length - 1)
          (Six(trie2), trie.last.last.last.last.last)
        }
      } else {
        val trie2 = copy(trie)
        trie2(trie2.length - 1) = copy(trie2.last, trie2.last.length - 1)
        trie2.last(trie2.last.length - 1) = copy(trie2.last.last, trie2.last.last.length - 1)
        trie2.last.last(trie2.last.last.length - 1) = copy(trie2.last.last.last, trie2.last.last.last.length - 1)
        trie2.last.last.last(trie2.last.last.last.length - 1) = copy(trie2.last.last.last.last, trie2.last.last.last.last.length - 1)
        (Six(trie2), trie.last.last.last.last.last)
      }
    }
  }
}
