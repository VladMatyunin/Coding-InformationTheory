package vlad.matyunin

/**
  * Created by Vlad on 17/12/17.
  */
class ArithmeticCoding(charTable: Map[String, Int], charNumber: Int, text:String) {
  private var data: List[Int] = List()
  val charProbability: Map[String, Double] = charTable.map((d)=>(d._1,d._2.toDouble/charNumber))

  val initialRaw: Raw = getRawBySegment((0.0, 0.99999))

  type Segment = (Double, Double)

  type Raw = Map[String, Segment]

  private def getRawBySegment(segm:Segment): Raw = {
    def iter(start: Double, dict: Map[String, Double]): Map[String, Segment] = dict.toList match {
      case List() => Map()
      case a::list => iter(start + dict.head._2, dict.tail) + (dict.head._1 -> (start, start + dict.head._2))
    }
    iter(segm._1, charProbability)
  }

  private def scale(segm: Segment): Segment = {
    val d1 = segm._1.toString.split(",")(1)
    val d2 = segm._2.toString.split(",")(1)
    if (d1(0) != d2(0)) segm
    else scale((segm._1 * 10, segm._2 * 10))
  }

  def encode(raw: Raw, data:List[String], acc:List[Int]):List[Int] = data match {
    case List() => acc
    case head::tail =>
      val newCode = encodeChar1(getSegment(raw,head.charAt(0)))
      encode(getRawBySegment(getSegment(raw, head.charAt(0))), tail, acc:::newCode)
  }
  private def encodeChar1(segment: Segment):List[Int] = {
    scale(segment)._1.toInt.toString.map(_.toInt).toList
  }

  private def getSegment(raw: Raw, char: Char): Segment = raw(char.toString)
}
