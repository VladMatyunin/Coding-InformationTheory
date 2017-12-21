package vlad.matyunin

/**
  * Created by Vlad on 17/12/17.
  */
class ArithmeticCoding(charTable: Map[String, Int], charNumber: Int, text:String, conditional:Boolean) {

  private val initialSegment = (0.0, 0.99999)
  private val initialRaw: Raw = getRawBySegment(initialSegment)

  //val result = encode(initialRaw, text.toList.map(c=>c.toString),List(), initialSegment)
  val result = if (!conditional) encode(text.toList.map(c=>c.toString)) else {
    var data = List[String]()
    for (i <- 0 until text.length-1 by 2){
      var b = text(i).toString+text(i+1)
      data = b::data
    }
    encode(data)
  }

  type Segment = (Double, Double)

  type Raw = Map[String, Segment]

  private def getRawBySegment(segm:Segment): Raw = {
    def iter(start: Double, dict: Map[String, Double]): Map[String, Segment] = dict.toList match {
      case List() => Map()
      case a::list => iter(start + dict.head._2, dict.tail) + (dict.head._1 -> (start, start + dict.head._2))
    }

    val charProbability: Map[String, Double] = charTable.map((d)=>(d._1,(segm._2-segm._1)*d._2.toDouble/charNumber))
    iter(segm._1, charProbability)
  }

  private def scale(segm: Segment): Segment = {
    val d1 = segm._1.toString.split('.')(1)
    val d2 = segm._2.toString.split('.')(1)
    if (d1(0) != d2(0)) segm
    else scale((segm._1 * 10, segm._2 * 10))
  }

  def encode(data: List[String]) : (List[Int], Segment) = {
    var currentSegment = initialSegment
    var acc = List[Int]()
    for(char<-data){
      val updatedRaw = getRawBySegment(currentSegment)
      val execResult =
        if (!conditional) encodeChar1(getSegment(updatedRaw, char.charAt(0).toString))
        else encodeChar1(getSegment(updatedRaw, char))
      acc = acc:::execResult._1
      currentSegment = execResult._2
    }
    (acc, currentSegment)
  }

  def encode(raw: Raw, data:List[String], acc:List[Int],currentSegm: Segment):(List[Int], Segment) = data match {
    case List() => (acc, currentSegm)
    case head::tail =>
      val updatedRaw = getRawBySegment(currentSegm)
      val execResult = encodeChar1(getSegment(updatedRaw, head.charAt(0).toString))
      encode(updatedRaw, tail, acc:::execResult._1, execResult._2)
  }
  private def encodeChar1(segment: Segment):(List[Int], Segment) = {
    val newSegment = scale(segment)
    if(newSegment._1.toInt>0)
      (newSegment._1.toInt.toString.map(_-'0').toList, createSegment(newSegment._1, newSegment._2))
    else (List(), newSegment)
  }

  private def getSegment(raw: Raw, char: String): Segment = raw(char)

  private def createSegment(d1:Double, d2:Double): Segment = {
    def reduceLeft(value:Double): Double = ("0." + value.toString.split('.')(1)).toDouble
    if (d1>d2) (reduceLeft(d2),reduceLeft(d2))
    else (reduceLeft(d1), reduceLeft(d2))
  }
}