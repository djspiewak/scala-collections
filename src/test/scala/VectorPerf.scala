import com.codecommit.collection.Vector

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.IntMap

object VectorPerf {
  import PerfLib._
  
  def main(args: Array[String]) {
    println()
    
    //==========================================================================
    {
      title("Fill 100000 Sequential Indexes")
      
      val vectorOp = "Vector" -> time {
        var vec = Vector[Int]()
        var i = 0
        
        while (i < 100000) {
          vec += i
          i += 1
        }
      }
      
      val arrayOp = "ArrayBuffer" -> time {
        var arr = new ArrayBuffer[Int]
        var i = 0
        
        while (i < 100000) {
          arr += i
          i += 1
        }
      }
      
      vectorOp compare arrayOp
      
      val intMapOp = "IntMap" -> time {
        var map = IntMap[Int]()
        var i = 0
        
        while (i < 100000) {
          map = map(i) = i
          i += 1
        }
      }
      
      vectorOp compare intMapOp
      
      val oldIntMapOp = "Map[Int, _]" -> time {
        var map = Map[Int, Int]()
        var i = 0
        
        while (i < 100000) {
          map = map(i) = i
          i += 1
        }
      }
      
      vectorOp compare oldIntMapOp
      
      div('=')
      println()
    }
    
    //==========================================================================
    {
      title("Read 100000 Sequential Indexes")
      
      var vec = Vector[Int]()
      for (i <- 0 until 100000) {
        vec += i
      }
      
      var arr = new ArrayBuffer[Int]
      for (i <- 0 until 100000) {
        arr += i
      }
      
      var map = IntMap[Int]()
      for (i <- 0 until 100000) {
        map = map(i) = i
      }
      
      var oldMap = Map[Int, Int]()
      for (i <- 0 until 100000) {
        oldMap = oldMap(i) = i
      }
      
      val vectorOp = "Vector" -> time {
        var i = 0
        while (i < vec.length) {
          vec(i)
          i += 1
        }
      }
      
      val arrayOp = "ArrayBuffer" -> time {
        var i = 0
        while (i < arr.size) {
          arr(i)
          i += 1
        }
      }
      
      vectorOp compare arrayOp
      
      val intMapOp = "IntMap" -> time {
        var i = 0
        while (i < vec.length) {    // map.size is unsuitable
          map(i)
          i += 1
        }
      }
      
      vectorOp compare intMapOp
      
      val oldIntMapOp = "Map[Int, _]" -> time {
        var i = 0
        while (i < vec.length) {    // map.size is unsuitable
          oldMap(i)
          i += 1
        }
      }
      
      vectorOp compare oldIntMapOp
      
      div('=')
      println()
    }
    
    //==========================================================================
    {
      title("Read 100000 Random Indexes")
      
      val indexes = new Array[Int](100000)
      var max = -1
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.round(Math.random * 40000000).toInt
        max = Math.max(max, indexes(i))
      }
      
      var vec = Vector[Int]()
      for (i <- 0 to max) {     // unplesant hack
        vec += 0
      }
      
      for (i <- 0 until indexes.length) {
        vec = vec(indexes(i)) = i
      }
      
      val arr = new ArrayBuffer[Int]
      for (i <- 0 to max) {     // unplesant hack
        arr += 0
      }
      
      for (i <- 0 until indexes.length) {
        arr(indexes(i)) = i
      }
      
      var map = IntMap[Int]()
      for (i <- 0 until indexes.length) {
        map = map(indexes(i)) = i
      }
      
      var oldMap = Map[Int, Int]()
      for (i <- 0 until indexes.length) {
        oldMap = map(indexes(i)) = i
      }
      
      val vectorOp = "Vector" -> time {
        var i = 0
        while (i < indexes.length) {
          vec(indexes(i))
          i += 1
        }
      }
      
      val arrayOp = "ArrayBuffer" -> time {
        var i = 0
        while (i < indexes.length) {
          arr(indexes(i))
          i += 1
        }
      }
      
      vectorOp compare arrayOp
      
      val intMapOp = "IntMap" -> time {
        var i = 0
        while (i < indexes.length) {
          map(indexes(i))
          i += 1
        }
      }
      
      vectorOp compare intMapOp
      
      val oldIntMapOp = "Map[Int, _]" -> time {
        var i = 0
        while (i < indexes.length) {
          oldMap(indexes(i))
          i += 1
        }
      }
      
      vectorOp compare oldIntMapOp
      
      div('=')
      println()
    }
    
    //==========================================================================
    {
      title("Reverse of Length 100000")
      
      var vec = Vector[Int]()
      for (i <- 0 until 100000) {
        vec += i
      }
      
      var arr = new ArrayBuffer[Int]
      for (i <- 0 until 100000) {
        arr += i
      }
      
      var map = IntMap[Int]()
      for (i <- 0 until 100000) {
        map = map(i) = i
      }
      
      val vectorOp = "Vector" -> time {
        vec.reverse
      }
      
      val arrayOp = "ArrayBuffer" -> time {
        arr.reverse
      }
      
      vectorOp compare arrayOp
      
      div('=')
      println()
    }
    
    //==========================================================================
    {
      title("Compute Length (100000)")
      
      var vec = Vector[Int]()
      for (i <- 0 until 100000) {
        vec += i
      }
      
      var arr = new ArrayBuffer[Int]
      for (i <- 0 until 100000) {
        arr += i
      }
      
      var map = IntMap[Int]()
      for (i <- 0 until 100000) {
        map = map(i) = i
      }
      
      var oldMap = Map[Int, Int]()
      for (i <- 0 until 100000) {
        oldMap = oldMap(i) = i
      }
      
      val vectorOp = "Vector" -> time {
        vec.length
      }
      
      val arrayOp = "ArrayBuffer" -> time {
        arr.length
      }
      
      vectorOp compare arrayOp
      
      val intMapOp = "IntMap" -> time {
        map.size
      }
      
      vectorOp compare intMapOp
      
      val oldIntMapOp = "Map[Int, _]" -> time {
        oldMap.size
      }
      
      vectorOp compare oldIntMapOp
      
      div('=')
      println()
    }
  }
}

