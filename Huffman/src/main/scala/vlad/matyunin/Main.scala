package vlad.matyunin

import java.io.File
import java.util
import java.util.{Comparator, Date}

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Vlad on 12.11.2017.
  */
object Main extends App {
  override def main(args: Array[String]): Unit = {
    if (args(0) == null) throw new Exception("No file provided")

    var map = new mutable.TreeMap[String, Int]()
    var file = new File(args(0))
    var charNumber = 0

    var text = ""
    for (lines <- Source.fromFile(file).getLines) {
      text+=lines
      lines.foreach((b) => {
        if (map.contains(b.toString))
          map(b.toString) += 1
        else map.put(b.toString, 1)
        charNumber+=1
      })
    }
    val hufTree = new HuffmanTree(map.toMap)
    hufTree.execute()
    new FileWriter().writeToFile(args(0)+".zizip",hufTree.getDictionary(),text, cond = false, null, charNumber)
    println("Entropy:"+findEntropy(map.toMap,charNumber))
    yslovnaya(file, charNumber)
    /*
    new HuffmanTree(map.toMap).execute()
    execute(Source.fromFile(file).getLines().next())
    execute("beepboopbeer!")*/
  }

  def findEntropy(dict: Map[String, Int], totalChars:Int) = {
    var entropy = 0.0
    for ((key, value)<-dict ){
      var p = value.toFloat/totalChars.toFloat
      entropy+=p* (Math.log(p)/Math.log(2.0))
    }
    entropy
  }
  def yslovnaya(file: File, charNumber: Int) = {
    var text = ""
    var map = new mutable.TreeMap[String, Int]()
    for (lines <- Source.fromFile(file).getLines) {
      text+=lines
      for (i <- 0 until lines.length-1){
        var b = lines(i).toString+lines(i+1)
        if (map.contains(b))
          map(b) += 1
        else map.put(b, 1)
      }
    }
    val hufTree = new HuffmanTree(map.toMap)
    hufTree.execute()
    new FileWriter().writeToFile(file.getAbsolutePath+"yslovnaya.zizip",hufTree.getDictionary(),text, cond=true,file, charNumber)
  }
}

