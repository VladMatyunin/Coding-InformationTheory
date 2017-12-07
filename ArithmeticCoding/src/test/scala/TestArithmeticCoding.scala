import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import vlad.matyunin.ArithmeticCoding

/**
  * Created by Vlad on 04.12.2017.
  */
@RunWith(classOf[JUnitRunner])
class TestArithmeticCoding extends FunSuite{
  /*test("check text data") {
    val coder = new ArithmeticCoding("aab baa")
    assert(coder.textData._2==7)
    assert(coder.textData._1('a')==4)
    assert(coder.textData._1.size==3)
  }
  test("test initial ray"){
    val coder = new ArithmeticCoding("aab baa cc")
    assert(coder.initialRay.size==4)
    val segm = coder.initialRay('a')
    assert(segm._2-segm._1-4.toDouble/10<0.001)
  }
  test("Test ray by segment"){
    val coder = new ArithmeticCoding("aab baa cc")
    val aSegm = coder.getRay((0.0d,4.toDouble/10))('a')
    assert(aSegm._2-aSegm._1-(4*4).toDouble/(10*10)<0.0001)
  }*/
}
