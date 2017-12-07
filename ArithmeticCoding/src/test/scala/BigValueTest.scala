import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import vlad.matyunin.BigValue

/**
  * Created by Vlad on 06.12.2017.
  */
@RunWith(classOf[JUnitRunner])
class BigValueTest extends FunSuite{
  test("simple sum") {
    val value1 = new BigValue(List(0,1,1))
    val value2 = new BigValue(List(0,1,1))
    print((value1+value2).v)
    assert((value1+value2).v.equals(List(0,2,2)))
  }
  test("sum with 9 at the beginning") {
    val value1 = new BigValue(List(0,9,1))
    val value2 = new BigValue(List(0,1,1))
    print((value1+value2).v)
    assert((value1+value2).v.equals(List(1,0,2)))
  }
  test("sum with 9 at the end") {
    val value1 = new BigValue(List(0,1,9))
    val value2 = new BigValue(List(0,9,1))
    print((value1+value2).v)
    assert((value1+value2).v.equals(List(1,1,0)))
  }
  test("simple multiply by int 2") {
    val value1 = new BigValue(List(0,1,1))
    print((value1*2).v)
    assert((value1*2).v.equals(List(0,2,2)))
  }
  test("simple multiply by int 9") {
    val value1 = new BigValue(List(0,2,2))
    print((value1*9).v)
    assert((value1*9).v.equals(List(1,9,8)))
  }
  test("simple minus big value") {
    val value1 = new BigValue(List(0,2,2))
    val value2 = new BigValue(List(0,1,1))
    assert((value1-value2).v.equals(List(0,1,1)))
  }
  test("minus with problems, big value") {
    val value1 = new BigValue(List(0,2,2,5))
    val value2 = new BigValue(List(0,1,9,6))
    print((value1-value2).v)
    assert((value1-value2).v.equals(List(0,0,2,9)))
  }
  test("delete front zeros") {
    val value1 = new BigValue(List(0,0,0,1,0,1))
    assert(value1.deleteBeginZeros.v.equals(List(1,0,1)))
  }
  test("delete end zeros") {
    val value1 = new BigValue(List(0,1,1,0,0))
    assert(value1.deleteEndZeros.v.equals(List(0,1,1)))
  }
  test("simple multiply by double") {
    val value1 = new BigValue(List(0,1,1))
    print((value1*0.2).v)
    assert((value1*0.2).v.equals(List(0,0,2,2)))
  }
  test("complicated multiply by double") {
    val value1 = new BigValue(List(0,1,1))
    print((value1*0.11).v)
    assert((value1*0.11).v.equals(List(0,0,1,2,1)))
  }
  test("_zeros") {
    val value1 = new BigValue(List(0,1,1))
    print(value1._zeros(2).v)
    assert(value1._zeros(2).v.equals(List(0,1,1,0,0)))
  }
  test("zeros_") {
    val value1 = new BigValue(List(0,1,1))
    print(value1.zeros_(2).v)
    assert(value1.zeros_(2).v.equals(List(0,0,0,1,1)))
  }
}
