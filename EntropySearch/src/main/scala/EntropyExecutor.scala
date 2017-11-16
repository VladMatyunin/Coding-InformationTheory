import java.io.File
import java.util
import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * Created by Vlad on 03.11.2017.
  */
class EntropyExecutor {
  private val entropyMap = new mutable.HashMap[Char, mutable.HashMap[String, Int]]
  def executeEntropy(file: File) = {
    val (textLines, charTable, charCount)= new FileReader().readFile(file)
    for ((key,value)<-charTable) entropyMap.put(key,new mutable.HashMap[String, Int])

    var combinationCount = 0

    for(line <- textLines.asScala) {
      var charArray = line
      for (i<-0 until charArray.length-1){
        val entry = entropyMap(charArray(i))
        val key = charArray(i).toString + charArray(i + 1)

        if (entry.contains(key)) entry(key)+=1
        else entry.put(key, 1)
        combinationCount+=1
      }
    }
    var conditionalEntropy = 0.0

    for ((character, charMap)<-entropyMap){
      for((key,value)<-charMap){
        val oneCharValue = charTable.get(key(1))

        conditionalEntropy+=(value.toFloat/combinationCount) *
          (Math.log((value.toFloat / combinationCount) / (oneCharValue.get.toFloat / charCount))/Math.log(2.toDouble))
      }
    }
    (entropy(charTable, charCount), conditionalEntropy)
  }
  private def entropy(charMap: mutable.HashMap[Char, Int], totalCount:Int ): Double = {
    var entropy = 0d
    for((key,value)<-charMap) {
      entropy += (value.toFloat/totalCount) * (Math.log(value.toFloat/totalCount) / Math.log(2))
    }
    entropy
  }
}
