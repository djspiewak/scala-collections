import com.codecommit.collection.HashMap

object HashPerf {
  import PerfLib._
  
  def main(args: Array[String]) {
    println()
    
    val data = "The quick brown fox jumped over the lazy red dog"
    
    //==========================================================================
    {
      title("Fill 100000 Random Keys")
      
      val indexes = new Array[Int](100000)
      var max = -1
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.round(Math.random * 40000000).toInt - 20000000
        max = Math.max(max, indexes(i))
      }
      
      val hashMapOp = "HashMap" -> time {
        var map = HashMap[Int, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(indexes(i)) = data
          i += 1
        }
      }
      
      val mapOp = "Map" -> time {
        var map = Map[Int, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(indexes(i)) = data
          i += 1
        }
      }
      
      hashMapOp compare mapOp
      
      val intMapOp = "IntMap" -> time {
        var map = test.IntMap[String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(indexes(i)) = data
          i += 1
        }
      }
      
      hashMapOp compare intMapOp
      
      val mutableMapOp = "mutable.Map" -> time {
        val map = scala.collection.mutable.Map[Int, String]()
        var i = 0
        
        while (i < indexes.length) {
          map(indexes(i)) = data
          i += 1
        }
      }
      
      hashMapOp compare mutableMapOp
      div('=')
    }
    
    println()
    
    //==========================================================================
    {
      title("Read 100000 Random Keys")
      
      val indexes = new Array[Int](100000)
      var max = -1
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.round(Math.random * 40000000).toInt - 20000000
        max = Math.max(max, indexes(i))
      }
      
      var hashMap = HashMap[Int, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          hashMap = hashMap(indexes(i)) = data
          i += 1
        }
      }
      
      var immutableMap = Map[Int, String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          immutableMap = immutableMap(indexes(i)) = data
          i += 1
        }
      }
      
      
      val hashMapOp = "HashMap" -> time {
        var i = 0
        while (i < indexes.length) {
          hashMap(indexes(i))
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
      
      hashMapOp compare mapOp
      
      var intMap = test.IntMap[String]()
      
      {
        var i = 0
        
        while (i < indexes.length) {
          intMap = intMap(indexes(i)) = data
          i += 1
        }
      }
      
      val intMapOp = "IntMap" -> time {
        var i = 0
        while (i < indexes.length) {
          intMap(indexes(i))
          i += 1
        }
      }
      
      hashMapOp compare intMapOp
      
      val mutableMap = scala.collection.mutable.Map[Int, String]()
      
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
      
      hashMapOp compare mutableMapOp
      div('=')
    }
  }
}
