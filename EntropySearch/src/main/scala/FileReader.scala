import java.io.File
import java.util

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Vlad on 03.11.2017.
  */
class FileReader {
  def readFile(file: File): (util.LinkedList[String], mutable.HashMap[Char, Int], Int) = {
    val totalLines = new util.LinkedList[String]
    val charTable = new mutable.HashMap[Char, Int]()
    var totalNumber = 0
    for (line <- Source.fromFile(file).getLines()){
      totalLines.add(line)
      line map(a=> {
        totalNumber += 1
        if (charTable.contains(a)){
          charTable(a)+=1
        } else charTable.put(a, 1)
      })
    }
    (totalLines, charTable, totalNumber)
  }
}
