package vlad.matyunin

import java.io.File

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.Source

/**
  * Created by Vlad on 18.11.2017.
  */
object Starter extends App{
  override def main(args: Array[String]): Unit = {
    var map = new mutable.TreeMap[(String,String), Int]()
    var file = new File(args(0))
    var charNumber = 0

    var text = ""
    for (lines <- Source.fromFile(file).getLines) {
      text+=lines
      lines.foreach((b) => {
        if (map.contains((b.toString,"")))
          map((b.toString,"")) += 1
        else map.put((b.toString,""), 1)
        charNumber+=1
      })
    }
    val result = execute(map.toMap,' ')
    new FileWriter().writeToFile(args(0)+".zizip",toCodeDict(result),text,false,file,charNumber)
    yslovnaya(file,charNumber, text)

  }
  def yslovnaya(file:File, charNumber:Int, text:String) = {
    var map = new mutable.TreeMap[(String,String), Int]()
      for (lines <- Source.fromFile(file).getLines) {
        for (i <- 0 until lines.length-1){
          var b = lines(i).toString+lines(i+1)
          if (map.contains((b,"")))
            map((b,""))+=1
          else map.put((b,""), 1)
        }
      }
    val result = execute(map.toMap,' ')
    new FileWriter().writeToFile(file.getAbsolutePath+"ysl.zizip",toCodeDict(result),text,true,file,charNumber)
  }

  def execute(dict :Map[(String,String), Int], code:Char):Map[(String, String), Int] = {
    if (dict.size<=1)
      dict
    else {
      val sortedMap = ListMap(dict.toSeq.sortWith(_._2 > _._2):_*)
      var modifiedDict = new mutable.HashMap[(String, String), Int]
      for ((key, value) <- sortedMap) {
        val updatedKey = (key._1, key._2.concat(code.toString))
        modifiedDict.+=(updatedKey->value)
      }

      val divDict = divide(modifiedDict.toMap)
      merge(execute(divDict._1,'0'),execute(divDict._2, '1'))
    }
  }
  private def merge(dict1:Map[(String,String), Int], dict2:Map[(String,String), Int]) = {
    var dictResult = new mutable.HashMap[(String, String), Int]()
    for ((key, value)<-dict2)
      dictResult.+=(key->value)
    for ((key, value)<-dict1)
      dictResult.+=(key->value)
    dictResult.toMap
  }
  private def divide(dict:Map[(String, String), Int]) = {
    var sum = 0
    for ((_, value)<-dict)
      sum+=value
    val mediumValue = sum/2
    var leftDict = new mutable.HashMap[(String, String), Int]()
    var rightDict = new mutable.HashMap[(String, String), Int]()
    val dictArray = dict.toArray.sortWith(_._2 > _._2)
    var counter = 0
    var status = 0
    while (counter<mediumValue){
      leftDict+=dictArray(status)
      counter+=dictArray(status)._2
      status+=1
    }
    for (i<-status until dictArray.length){
      rightDict+=dictArray(i)
    }
    (leftDict.toMap, rightDict.toMap)
  }
  def toCodeDict(dict: Map[(String, String), Int]):mutable.HashMap[String, String] = {
    val codeDict = new mutable.HashMap[String, String]
    for ((key,value)<-dict) {
      codeDict.put(key._1, key._2)
    }
    codeDict
  }
}
