package vlad.matyunin

import java.util

import scala.collection.mutable

/**
  * Created by Vlad on 14.11.2017.
  */
class HuffmanNode(left: HuffmanNode = null, right: HuffmanNode = null, value: String, path: Int) extends Comparable[HuffmanNode] {
  val charValue: String = value
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

class HuffmanTree(data: Map[String, Int]) {

  var tree = new util.ArrayList[HuffmanNode]
  data.foreach(item => {
    tree.add(new HuffmanNode(value = item._1.toString, path = item._2))
  })

  def execute(): Unit = {
    while (tree.size() > 1) {
      tree.sort((o1: HuffmanNode, o2: HuffmanNode) => {
        o1.compareTo(o2)
      })
      merge(tree.get(0), tree.get(1))
    }
  }



  private def getCode(c: String, huffmanNode: HuffmanNode, path: String): String = {
    if (huffmanNode==null) return ""
    if (huffmanNode.charValue == c) {
      return path
    }
    getCode(c, huffmanNode.leftNode, path + "0")+getCode(c, huffmanNode.rightNode, path + "1")
  }

  def getDictionary():mutable.HashMap[String, String] = {
    val dictionary = new mutable.HashMap[String, String]()
    def iterate(code: String, node: HuffmanNode):Unit = {
      if (node.charValue!=null) dictionary.put(node.charValue, code)
      else {
        if (node.leftNode!=null) iterate(code+"0", node.leftNode)
        if (node.rightNode!=null) iterate(code+"1", node.rightNode)
      }
    }
    iterate("", tree.get(0))
    dictionary
  }

  def getText(code: String):String = {
    val dict = getDictionary()
    var flag = true
    var text = ""
    var pointer = 0
    while (flag) {
      for ((key, value) <-dict ){
        if(code.substring(pointer,pointer+value.length)==value) {
          text+=key
          pointer+=value.length
        }
      }
      if (pointer==code.length) flag=false
    }
    text
  }

  private def merge(nodeA: HuffmanNode, nodeB: HuffmanNode): Unit = {
    tree.remove(nodeA)
    tree.remove(nodeB)
    if (nodeA.pathValue < nodeB.pathValue)
      tree.add(new HuffmanNode(left = nodeA, right = nodeB, path = nodeA.pathValue + nodeB.pathValue, value = null))
    else
      tree.add(new HuffmanNode(left = nodeB, right = nodeA, path = nodeA.pathValue + nodeB.pathValue, value = null))
  }
}
