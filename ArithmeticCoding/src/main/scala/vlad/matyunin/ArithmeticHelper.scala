package vlad.matyunin

import javax.swing.text.Segment

/**
  * Created by Vlad on 05.12.2017.
  */
object ArithmeticHelper {
  def formatSegment(left: String, right: String):(String,String) = {
    if (left.length < right.length)
      ((left.toList:::(for (_ <- 0 until  right.length - left.length) yield '0').toList).mkString, right)
    else if(right.length<left.length)
      (left,(right.toList:::(for (_ <- 0 until  left.length - right.length) yield '0').toList).mkString)
    else (left, right)
  }

  def getMinString(strMin: String, strMax: String, index: Int): String = {
    if (index + 4 < strMin.length) {
      val intMin = strMin.substring(index, index + 4).toInt
      val intMax = strMax.substring(index, index + 4).toInt
      val minInt = getMinInt(intMin, intMax)
      checkSize(strMin.substring(0,index)+minInt)
    }
    else {
      val intMin = strMin.substring(index, strMin.length).toInt
      val intMax = strMax.substring(index, strMax.length).toInt
      val minInt = getMinInt(intMin, intMax)
      checkSize(strMin.substring(0,index)+minInt)
    }
  }
  def checkSize(number:String):String = {
    if (number.length>6) number.substring(0,6)
    else number
  }

  def getMinInt(intMin: Int, intMax: Int): Int = {
    def check(min: Int, max: Int, incr: Int):Boolean = (min + incr - min % incr) < max

    if (check(intMin, intMax, 1000)) (intMin + (1000-intMin % 1000))/1000
    else if (check(intMin, intMax, 100)) (intMin + (100-intMin % 100))/100
    else if (check(intMin, intMax, 10)) (intMin + (10-intMin % 10))/10
    else intMax
  }
  def roundSegmValues(left: Double,right: Double,index:Int):(Double,Double) = {
    (roundDouble(left,index+2), roundDouble(right,index+2))
  }
  def roundDouble(value:Double, index:Int):Double =
    BigDecimal(value).setScale(index+2, BigDecimal.RoundingMode.HALF_UP).toDouble

}
