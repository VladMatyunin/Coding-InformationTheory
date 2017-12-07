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
  class Segment(leftBound:BigValue, rightBound:BigValue) {

    val left: BigValue = {
      if (leftBound.v.size >= rightBound.v.size) leftBound
      else leftBound._zeros(rightBound.v.size - leftBound.v.size)
    }
    val right: BigValue = {
      if (rightBound.v.size>leftBound.v.size) rightBound
      else rightBound._zeros(leftBound.v.size-rightBound.v.size)
    }

    val delta: BigValue = right - left
    val minimalMedium: BigValue = {
      var result = right
      for (i <- left.v.indices) {
        if (left.v(i)!=right.v(i)){
          result = new BigValue(left.v.slice(0,i+1).updated(i,left.v(i)+1))
        }
      }
      result
    }

    override def toString: String = {
      left.toString+" -- "+right.toString
    }
  }



  type Ray = Map[Char, Segment]

  val initialRay: Ray = getRay(new Segment(new BigValue(List(0)), new BigValue(List(9,9,9,9,9,9,9,9))))

  def getRay(segment: Segment): Ray = {
    val totalCharCount = textData._2
    val delta = segment.delta
    val charProbability = textData._1.map((obj) => (obj._1, delta*(obj._2.toDouble /totalCharCount)))

    def iter(start: BigValue, dict: Map[Char, BigValue]): Map[Char, Segment] = dict.toList match {
      case List() => Map()
      case a::list => iter(start + dict.head._2, dict.tail) + (dict.head._1 -> new Segment(start, start + dict.head._2))
    }

    iter(segment.left, charProbability)
  }

  def execute():List[(Char,BigValue)] = {
    var currentRay = initialRay
    var dict = List[(Char, BigValue)]()
    for (char <- text.toCharArray) {
      dict = (char, encode(currentRay, char))::dict
      currentRay = getRay(currentRay(char))
    }
    dict
  }

  def getMinValue(segment: Segment): BigValue =
    segment.minimalMedium


  def encode(currentRay: Ray, char: Char): BigValue = {
    getMinValue(currentRay(char))
  }


  def doubleToBytes(value: BigValue): String = {
    var result = ""
    value.v.foreach((v)=>result+=v.toBinaryString)
    result
  }
}
