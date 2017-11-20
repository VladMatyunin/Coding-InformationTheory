package vlad.matyunin

import java.io._
import java.util

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Vlad on 14.11.2017.
  */
class FileWriter {
  def writeToFile(path: String, dict: mutable.HashMap[String, String], text: String, cond: Boolean, origin:File, charNumber:Int): Unit = {
    val file = new File(path)
    file.createNewFile()
    val stream = new FileOutputStream(file)
    val dictString = dict.foldLeft("") { (s: String, pair: (String, String)) =>
      s + pair._1 + pair._2
    }
    var hufIndex = 0f
    var zeroOneNumber = 0
    //stream.write(dictString.getBytes())
    if (cond) {
      val code = getCode2(new File(origin.getAbsolutePath), dict)
      zeroOneNumber+=code.length
      hufIndex = zeroOneNumber.toFloat/charNumber
      print("Huffman cond: "+hufIndex)
      stream.write(convert(code).toByteArray)
    }
    else {

      text.grouped(100).foreach((s) => {
        val code = getCode(s, dict)
        zeroOneNumber+=code.length
        stream.write(convert(code).toByteArray)
      })
      hufIndex = zeroOneNumber.toFloat/charNumber
      print("Huffman: "+hufIndex)
    }
  }
  def getCode(text: String, dict: mutable.HashMap[String, String]): String = {
    var result = ""
    for (chars <- text.toCharArray) {
      result += dict(chars.toString)
    }
    result
  }

  def getCode2(file:File, dict: mutable.HashMap[String, String]): String = {
    var result = ""
    for (lines <- Source.fromFile(file).getLines) {
      var i = 0
      while (i < lines.length-1) {
        try {
          result += dict(lines(i).toString + lines(i + 1))
          i+=2
        }
        catch {
          case e: Exception => println("error")
        }
      }
    }
    result
  }
  private def convert(s:String) : util.BitSet = {
    val bitSet = new util.BitSet(s.length)
    var bitcounter = 0
    for (c <- s.toCharArray) {
      if (c == '1') bitSet.set(bitcounter)
      bitcounter += 1
    }
    bitSet
  }

}

