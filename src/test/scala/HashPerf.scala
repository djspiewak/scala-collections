import com.codecommit.collection.HashTrie

import scala.collection.immutable.TreeHashMap

object HashPerf {
  import PerfLib._
  
  def main(args: Array[String]) {
    println()
    
    val data = "The quick brown fox jumped over the lazy red dog"
    
    //==========================================================================
    {
      title("Fill 100000 Random Keys")
      
      val indexes = new Array[String](100000)
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.random.toString
      }
      
      val hashTrieOp = "HashTrie" -> time {
        var map = HashTrie[String, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(indexes(i)) = data
          i += 1
        }
      }
      
      val mapOp = "Map" -> time {
        var map = Map[String, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(indexes(i)) = data
          i += 1
        }
      }
      
      hashTrieOp compare mapOp
      
      /* val intMapOp = "TreeHashMap" -> time {
        var map = TreeHashMap[String, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(indexes(i)) = data
          i += 1
        }
      }
      
      hashTrieOp compare intMapOp */
      
      val mutableMapOp = "mutable.Map" -> time {
        val map = scala.collection.mutable.Map[String, String]()
        var i = 0
        
        while (i < indexes.length) {
          map(indexes(i)) = data
          i += 1
        }
      }
      
      hashTrieOp compare mutableMapOp
      div('=')
    }
    
    println()
    
    //==========================================================================
    {
      title("Read 100000 Random Keys")
      
      val indexes = new Array[String](100000)
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.random.toString
      }
      
      var hashTrie = HashTrie[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          hashTrie = hashTrie(indexes(i)) = data
          i += 1
        }
      }
      
      var immutableMap = Map[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          immutableMap = immutableMap(indexes(i)) = data
          i += 1
        }
      }
      
      
      val hashTrieOp = "HashTrie" -> time {
        var i = 0
        while (i < indexes.length) {
          hashTrie(indexes(i))
          i += 1
        }
      }
      
      val mapOp = "Map" -> time {
        var i = 0
        while (i < indexes.length) {
          immutableMap(indexes(i))
          i += 1
        }
      }
      
      hashTrieOp compare mapOp
      
      /* var intMap = TreeHashMap[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          intMap = intMap(indexes(i)) = data
          i += 1
        }
      }
      
      val intMapOp = "TreeHashMap" -> time {
        var i = 0
        while (i < indexes.length) {
          intMap(indexes(i))
          i += 1
        }
      }
      
      hashTrieOp compare intMapOp */
      
      val mutableMap = scala.collection.mutable.Map[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          mutableMap(indexes(i)) = data
          i += 1
        }
      }
      
      val mutableMapOp = "mutable.Map" -> time {
        var i = 0
        while (i < indexes.length) {
          mutableMap(indexes(i))
          i += 1
        }
      }
      
      hashTrieOp compare mutableMapOp
      div('=')
    }
    
    println()
    
    //==========================================================================
    {
      title("Remove 50000 Random Keys")
      
      val indexes = new Array[String](100000)
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.random.toString
      }
      
      var hashTrie = HashTrie[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          hashTrie = hashTrie(indexes(i)) = data
          i += 1
        }
      }
      
      var immutableMap = Map[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          immutableMap = immutableMap(indexes(i)) = data
          i += 1
        }
      }
      
      
      val hashTrieOp = "HashTrie" -> time {
        var i = 0
        var map = hashTrie
        while (i < indexes.length) {
          map -= indexes(i)
          i += 1
        }
      }
      
      val mapOp = "Map" -> time {
        var i = 0
        var map = immutableMap
        
        while (i < indexes.length) {
          immutableMap -= indexes(i)
          i += 1
        }
      }
      
      hashTrieOp compare mapOp
      
      /* var intMap = TreeHashMap[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          intMap = intMap(indexes(i)) = data
          i += 1
        }
      } */
      
      /* val intMapOp = "TreeHashMap" -> time {
        var i = 0
        var map = intMap
        
        while (i < indexes.length) {
          map -= indexes(i)
          i += 1
        }
      }
      
      hashTrieOp compare intMapOp */
      
      div('=')
    }
    
    println()
    
    //==========================================================================
    {
      title("Loop Over 100000 Random Keys (#foreach)")
      
      val indexes = new Array[String](100000)
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.random.toString
      }
      
      var hashTrie = HashTrie[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          hashTrie = hashTrie(indexes(i)) = data
          i += 1
        }
      }
      
      var immutableMap = Map[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          immutableMap = immutableMap(indexes(i)) = data
          i += 1
        }
      }
      
      
      val hashTrieOp = "HashTrie" -> time {
        hashTrie foreach { case (k, v) => () }
      }
      
      val mapOp = "Map" -> time {
        immutableMap foreach { case (k, v) => () }
      }
      
      hashTrieOp compare mapOp
      
      /* var intMap = TreeHashMap[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          intMap = intMap(indexes(i)) = data
          i += 1
        }
      }
      
      val intMapOp = "TreeHashMap" -> time {
        intMap foreach { case (k, v) => () }
      }
      
      hashTrieOp compare intMapOp */
      
      val mutableMap = scala.collection.mutable.Map[String, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          mutableMap(indexes(i)) = data
          i += 1
        }
      }
      
      val mutableMapOp = "mutable.Map" -> time {
        mutableMap foreach { case (k, v) => () }
      }
      
      hashTrieOp compare mutableMapOp
      div('=')
    }
  }
}
