package vlad.matyunin

import java.io.File
import java.util
import java.util.Comparator

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Vlad on 12.11.2017.
  */
object Main extends App {
  override def main(args: Array[String]): Unit = {
    if (args(0) == null) throw new Exception("No file provided")
    var map = new mutable.TreeMap[Char, Int]()
    var file = new File(args(0))
    for (lines <- Source.fromFile(file).getLines) {
      lines.foreach((b) => {
        if (map.contains(b))
          map(b) += 1
        else map.put(b, 1)
      })
    }
    new HuffmanTree(map.toMap).execute()
    execute(Source.fromFile(file).getLines().next())
  }

  def execute(s: String): String = {
    var map = new mutable.TreeMap[Char, Int]()
    s.foreach((b) => {
      if (map.contains(b))
        map(b) += 1
      else map.put(b, 1)
    })
    val huffman = new HuffmanTree(map.toMap)
    huffman.execute()
    val path = huffman.getCode(s)
    print(path)
    path
  }
}

class HuffmanNode(left: HuffmanNode = null, right: HuffmanNode = null, value: Char, path: Int) extends Comparable[HuffmanNode] {
  val charValue: Char = value
  val pathValue: Int = path
  val leftNode: HuffmanNode = left
  val rightNode: HuffmanNode = right

  override def compareTo(o: HuffmanNode): Int = {
    this.pathValue - o.pathValue match {
      case x if x > 0 => 1
      case x if x < 0 => -1
      case x if x == 0 => 0
    }
  }
}

class HuffmanTree(data: Map[Char, Int]) {
  var tree = new util.ArrayList[HuffmanNode]
  data.foreach(item => {
    tree.add(new HuffmanNode(value = item._1, path = item._2))
  })

  def execute(): Unit = {
    while (tree.size() > 1) {
      tree.sort((o1: HuffmanNode, o2: HuffmanNode) => {
        o1.compareTo(o2)
      })
      merge(tree.get(0), tree.get(1))
    }
  }

  def getCode(text: String): String = {
    var result = ""
    for (chars <- text.toCharArray) {
      result += getCode(chars, tree.get(0), "")
    }
    result
  }

  def getCode(c: Char, huffmanNode: HuffmanNode, path: String): String = {
    if (huffmanNode==null) return ""
    if (huffmanNode.charValue == c) {
      return path
    }
    getCode(c, huffmanNode.leftNode, path + "0")+getCode(c, huffmanNode.rightNode, path + "1")
  }

  private def merge(nodeA: HuffmanNode, nodeB: HuffmanNode): Unit = {
    tree.remove(nodeA)
    tree.remove(nodeB)
    if (nodeA.pathValue < nodeB.pathValue)
      tree.add(new HuffmanNode(left = nodeA, right = nodeB, path = nodeA.pathValue + nodeB.pathValue, value = '0'))
    else
      tree.add(new HuffmanNode(left = nodeB, right = nodeA, path = nodeA.pathValue + nodeB.pathValue, value = '0'))
  }
}