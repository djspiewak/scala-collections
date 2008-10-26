package com.codecommit.collection

import java.io.{InputStream, OutputStream}
import java.util.Random

class BloomSetArray[+T] private (k: Int, contents: Array[Int]) {
  
  def this(width: Int, k: Int) = this(k, new Array[Int](width))
  
  def +[A >: T](e: A) = {
    val newContents = cloneArray(contents)
    add(newContents)(e)
    
    new BloomSetArray[A](k, newContents)
  }
  
  def ++[A >: T](col: Iterable[A]) = {
    val newContents = cloneArray(contents)
    val addNew = add(newContents) _
    
    col foreach addNew
    
    new BloomSetArray[A](k, newContents)
  }
  
  def contains[A >: T](e: A) = {
    val total = (0 until k).foldLeft(0) { (acc, i) => acc + contents(hash(e, i, contents.length)) }
    total == k
  }
  
  def store(os: OutputStream) {
    os.write(k)
    os.write(contents.length)
    contents foreach { os.write(_) }
  }
  
  private def add(contents: Array[Int])(e: Any) = {
    for (i <- 0 until k) {
      contents(hash(e, i, contents.length)) = 1
    }
  }
  
  private def hash(e: Any, iters: Int, bounds: Int): Int = if (iters == 0) 0 else {
    Math.abs(twist(e.hashCode + iters + hash(e, iters - 1, bounds)) % bounds)
  }
  
  private def twist(i: Int) = new Random(i).nextInt()
  
  private def cloneArray[A](a: Array[A]) = {
    val back = new Array[A](a.length)
    Array.copy(a, 0, back, 0, a.length)
    back
  }
}

object BloomSetArray {
  val DEFAULT_WIDTH = 200
  val DEFAULT_K = 4
  
  def apply[T](elems: T*) = {
    new BloomSetArray[T](DEFAULT_WIDTH, DEFAULT_K) ++ elems
  }
  
  def load[T](is: InputStream) = {
    val k = is.read()
    val contents = new Array[Int](is.read())
    
    for (i <- 0 until contents.length) {
      contents(i) = is.read()
    }
    
    new BloomSetArray[T](k, contents)
  }
}
