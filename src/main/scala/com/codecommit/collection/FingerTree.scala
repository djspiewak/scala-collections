package com.codecommit.collection

object FingerTree {
  sealed trait FingerTree[+A] {
    val isEmpty: Boolean
    
    def headLeft: A
    def tailLeft: FingerTree[A]
    
    def headRight: A
    def tailRight: FingerTree[A]
    
    def +:[B >: A](b: B): FingerTree[B]
    def +[B >: A](b: B): FingerTree[B]
    
    def viewLeft: FTViewLeft[FingerTree, A]
    def viewRight: FTViewRight[FingerTree, A]
    
    def iterator: Iterator[A]
  }
  
  case class Single[+A](a: A) extends FingerTree[A] {
    val headLeft = a
    val tailLeft = Empty
    
    val headRight = a
    val tailRight = Empty
    
    val isEmpty = false
    
    def +:[B >: A](b: B) = Deep(List(b), Empty, List(a))
    
    def +:[B >: A](node: Node[B]) = node match {
      case Node2(b, c) => Deep(List(b, c), Empty, List(a))
      case Node3(b, c, d) => Deep(List(b, c), Empty, List(d, a))
    }
    
    def +[B >: A](b: B) = Deep(List(a), Empty, List(b))
    
    def +[B >: A](node: Node[B]) = node match {
      case Node2(b, c) => Deep(List(a), Empty, List(b, c))
      case Node3(b, c, d) => Deep(List(a, b), Empty, List(c, d))
    }
    
    def viewLeft = FTConsLeft[FingerTree, A](a, Empty)
    def viewRight = FTConsRight[FingerTree, A](Empty, a)
    
    def iterator = new Iterator[A] {
      var hasNext = true
      
      def next = {
        hasNext = false
        a
      }
    }
    
    override def toString = "FingerTree(Single(%s))".format(a)
  }
  
  case class Deep[+A](prefix: List[A], tree: FingerTree[Node[A]], suffix: List[A]) extends FingerTree[A] {
    val isEmpty = false
    
    val headLeft = prefix.head
    val headRight = suffix.last
    
    def tailLeft = viewLeft.tail
    def tailRight = viewRight.tail
    
    def +:[B >: A](b: B) = prefix match {
      case d :: e :: f :: g :: Nil => Deep(List(b, d), Node3(d, e, f) +: tree, suffix)
      case partial => Deep(b :: partial, tree, suffix)
    }
    
    def +[B >: A](b: B) = suffix match {
      case g :: f :: e :: d :: Nil => Deep(prefix, tree + Node3(g, f, e), List(d, b))
      case partial => Deep(prefix, tree, partial ::: List(b))
    }
    
    def viewLeft = {
      def deep(prefix: List[A], tree: FingerTree[Node[A]], suffix: List[A]) = prefix match {
        case Nil => {
          tree.viewLeft match {
            case FTConsLeft(a, newTree) => Deep(a.toList, newTree, suffix)
            case FTNilLeft() => (suffix :\ (Empty: FingerTree[A])) { _ +: _ }
          }
        }
        
        case prefix => Deep(prefix, tree, suffix)
      }
      
      FTConsLeft(prefix.head, deep(prefix.tail, tree, suffix))
    }
    
    def viewRight = {
      def deep(prefix: List[A], tree: FingerTree[Node[A]], suffix: List[A]) = suffix match {
        case Nil => {
          tree.viewRight match {
            case FTConsRight(newTree, a) => Deep(prefix, newTree, a.toList)
            case FTNilRight() => (prefix :\ (Empty: FingerTree[A])) { _ +: _ }
          }
        }
        
        case suffix => Deep(prefix, tree, suffix)
      }
      
      FTConsRight(deep(prefix, tree, suffix dropRight 1), suffix.last)
    }
    
    def iterator = prefix.iterator ++ (tree.iterator flatMap { _.toList.iterator }) ++ suffix.iterator
    
    override def toString = "FingerTree(%s, %s, %s)".format(prefix, tree, suffix)
  }
  
  case object Empty extends FingerTree[Nothing] {
    val isEmpty = true
    
    def headLeft = throw new NoSuchElementException("headLeft on empty finger tree")
    def tailLeft = throw new NoSuchElementException("tailLeft on empty finger tree")
    
    def headRight = throw new NoSuchElementException("headRight on empty finger tree")
    def tailRight = throw new NoSuchElementException("tailRight on empty finger tree")
    
    def +:[A](a: A) = Single(a)
    
    def +[A](a: A) = Single(a)
    
    def viewLeft = FTNilLeft[FingerTree]()
    def viewRight = FTNilRight[FingerTree]()
    
    def iterator = new Iterator[Nothing] {
      val hasNext = false
      
      def next = throw new NoSuchElementException
    }
    
    override def toString = "FingerTree(Empty)"
  }
  
 
  sealed trait Node[+A] {
    def toList: List[A]
  }
  
  case class Node2[+A](a1: A, a2: A) extends Node[A] {
    def toList = List(a1, a2)
    
    override def toString = "Node2(%s, %s)".format(a1, a2)
  }
  
  case class Node3[+A](a1: A, a2: A, a3: A) extends Node[A] {
    def toList = List(a1, a2, a3)
    
    override def toString = "Node3(%s, %s, %s)".format(a1, a2, a3)
  }
  
  
  sealed trait FTViewLeft[+S[+_], +A] {
    def head: A
    def tail: S[A]
  }
  
  case class FTConsLeft[+S[+_], +A](head: A, tail: S[A]) extends FTViewLeft[S, A]
  
  case class FTNilLeft[+S[+_]]() extends FTViewLeft[S, Nothing] {
    def head = throw new NoSuchElementException("head on empty view")
    def tail = throw new NoSuchElementException("tail on empty view")
  }
  
  
  sealed trait FTViewRight[+S[+_], +A] {
    def tail: S[A]
    def head: A
  }
  
  case class FTConsRight[+S[+_], +A](tail: S[A], head: A) extends FTViewRight[S, A]
  
  case class FTNilRight[+S[+_]]() extends FTViewRight[S, Nothing] {
    def tail = throw new NoSuchElementException("tail on empty view")
    def head = throw new NoSuchElementException("head on empty view")
  }
}
