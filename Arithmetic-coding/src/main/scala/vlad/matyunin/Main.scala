package vlad

import java.io.{File, FileOutputStream}
import java.util

import vlad.matyunin.ArithmeticCoding

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Vlad on 17/12/17.
  */
object Main extends App{
  type Segment = (Double, Double)
  private def read(file:File): String = {
    //TODO: convert to immutable
    var text = ""
    for (lines <- Source.fromFile(file).getLines)
      text+=lines
    text
  }
  override def main(args: Array[String]): Unit = {
    val text = read(new File(args(0)))
    val output = new File(args(1))
    val map = new mutable.TreeMap[String, Int]()
    text.foreach((b) => {
      if (map.contains(b.toString))
        map(b.toString) += 1
      else map.put(b.toString, 1)
    })

    val encoder = new ArithmeticCoding(map.toMap,text.length,text, false)
    execute(encoder, text, output)

    var map2 = new mutable.TreeMap[String, Int]()
    for (i <- 0 until text.length-1){
      var b = text(i).toString+text(i+1)
      if (map2.contains(b))
        map2(b)+=1
      else map2.put(b, 1)
    }
    val encoder2 = new ArithmeticCoding(map2.toMap, text.length, text, true)
    execute(encoder2, text, new File(output.getAbsolutePath+".ysl"))
  }
  private def execute(encoder: ArithmeticCoding, text: String, output: File) = {
    val prefix = encoder.result._1
    val suffix = convert(encoder.result._2)
    val totalResult = prefix:::suffix
    val binary = toBinary(totalResult)
    val bitset = convert(binary)
    print("Bits per symbol:  "+bitset.length.toDouble/text.length)
    write(output, bitset)
  }
  private def convert(segm:Segment): List[Int] = {
    segm._1.toString.split('.')(0).map(_-'0').toList
  }
  private def toBinary(data: List[Int]):String = {
    data.map(_.toBinaryString).mkString
  }
  private def write(file:File, data: util.BitSet) = {
    val stream = new FileOutputStream(file)
    stream.write(data.toByteArray)
    stream.close()
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
