package vlad.matyunin

import scala.collection.mutable


class ArithmeticCoding(text:String) {
  val textData: (Map[Char, Int], Int, String) = {
    var map = new mutable.TreeMap[Char, Int]()
    var charNumber = 0
    text.foreach((b) => {
      if (map.contains(b))
        map(b) += 1
      else map.put(b, 1)
      charNumber += 1
    })
    (map.toMap, charNumber, text)
  }

  type Segment = (Double, Double)
  type Ray = Map[Char, Segment]

  val initialRay: Ray = getRay((0.0d, 0.999999999999d))

  def getRay(segment: Segment): Ray = {
    val totalCharCount = textData._2
    val delta = segment._2 - segment._1
    val charProbability = textData._1.map((obj) => (obj._1, obj._2.toDouble / totalCharCount * delta))

    def iter(start: Double, dict: Map[Char, Double]): Map[Char, Segment] = dict.toList match {
      case List() => Map()
      case a::list => iter(start + dict.head._2, dict.tail) + (dict.head._1 -> (start, start + dict.head._2))
    }

    val result = iter(segment._1, charProbability)
    var formattedRay = Map[Char,Segment]()
    result.foreach((obj)=>{
      val left = obj._2._1.toString.substring(2)
      val right = obj._2._2.toString.substring(2)
      val (text1, text2) = ArithmeticHelper.formatSegment(left, right)
      var iter = true
      if(text1.length>5)
      for {i <- 0 until text1.length if iter} {
        if (text1(i) != text2(i)) {
          val newLeft = ("0."+text1).toDouble
          val newRight = ("0."+text2).toDouble
          formattedRay= formattedRay+(obj._1->ArithmeticHelper.roundSegmValues(newLeft,newRight,i))
          iter = false
        }
      }
      if (!formattedRay.contains(obj._1)) formattedRay+=obj
    })
    formattedRay
  }

  def execute():List[(Char,String)] = {
    var currentRay = initialRay
    var dict = List[(Char, String)]()
    for (char <- text.toCharArray) {
      dict = (char, encode(currentRay, char))::dict
      currentRay = getRay(currentRay(char))
    }
    dict
  }

  def getMinValue(segment: Segment): Int = {
    val left = segment._1.toString.substring(2)
    val right = segment._2.toString.substring(2)
    val (text1, text2) = ArithmeticHelper.formatSegment(left, right)
    for (i <- 0 until text1.length) {
      if (text1(i) != text2(i)) {
        return ArithmeticHelper.getMinString(text1, text2, i).toInt
      }
    }
    segment._1.toInt
  }

  def encode(currentRay: Ray, char: Char): String = {
    val doubleValue = getMinValue(currentRay(char))
    doubleToBytes(doubleValue)
  }


  def doubleToBytes(value: Double): String = value.toInt.toBinaryString
}
