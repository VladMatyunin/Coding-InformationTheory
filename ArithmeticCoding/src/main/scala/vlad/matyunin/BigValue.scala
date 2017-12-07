package vlad.matyunin

/**
  * Created by Vlad on 06.12.2017.
  */
class BigValue(values: List[Int]) {
  def format(other: BigValue): (BigValue,BigValue) = {
    if (values.size > other.v.size) {
      (this,other._zeros(values.size - other.v.size))
    }
    else if (other.v.size > values.size) {
      (this._zeros(other.v.size-values.size), other)
    }
    else (this,other)
  }

  val v: List[Int] = values

  def +(other: BigValue): BigValue = {
    val (current,other2) = format(other)
    var result = current
    for (i <- other2.v.size - 1 to 0 by -1) result = result up(other2.v(i), i)
    result.deleteEndZeros
  }

  def up(inc: Int, index: Int): BigValue = {
    if (index < 0) new BigValue(inc :: v)
    else {
      val result = new BigValue(v.updated(index, (v(index) + inc) % 10))
      if ((v(index) + inc) > 9) result.up((v(index)+inc)/10, index - 1)
      else result
    }
  }

  def down(inc: Int, index: Int): BigValue = {
    if (index < 0) throw new Exception
    if (v(index) - inc < 0) new BigValue(down(1, index - 1).v.updated(index, v(index) + 9 - inc))
    else new BigValue(v.updated(index, v(index) - inc))
  }

  def *(inc: Int): BigValue = {
    var result = new BigValue(v)
    for (i <- result.v.size - 1 to 0 by -1) {
      result = new BigValue(result.v.updated(i, result.v(i) * inc))
    }
    for (i <- result.v.size - 1 to 0 by -1) {
      if (result.v(i) > 9) result = new BigValue(result.v.updated(i, result.v(i) % 10)).up(result.v(i) / 10, i - 1)
    }
    result
  }

  def _zeros(number: Int): BigValue = new BigValue(v ::: (for {_ <- 0 until number} yield 0).toList)

  def zeros_(number: Int): BigValue = new BigValue((for {_ <- 0 until number} yield 0).toList ::: v)

  def deleteEndZeros: BigValue = {
    if (v.isEmpty) this
    else if (v.last == 0) new BigValue(v.init).deleteEndZeros
    else this
  }

  def deleteBeginZeros: BigValue = {
    if (v.head != 0) this
    else new BigValue(v.tail).deleteBeginZeros
  }
  def deleteBeginZeros(number: Int) :BigValue = {
    if (number>0){
      new BigValue(v.tail).deleteBeginZeros(number-1)
    }
    else this
  }

  def *(inc: Double): BigValue = {
    var digits = inc.toString
    digits = digits.substring(2, digits.length)
    //if (digits.length>=6)digits = digits.substring(2,6)
    //else digits = digits.substring(2,digits.length)
    val value1 = new BigValue(v).zeros_(digits.length)
    var result = new BigValue(List()).zeros_(digits.length + value1.v.size)
    for (i <- digits.length - 1 to 0 by -1) {
      if (digits(i) != '0')
        result = result + value1.deleteBeginZeros(digits.length-1-i) * (digits(i) - '0')
    }
    result.deleteEndZeros
  }

  def -(other: BigValue): BigValue = {
    if (other.v.size > v.size) _zeros(other.v.size - v.size) - other
    else if (other.v.size < v.size) this - other._zeros(v.size - other.v.size)
    else {
      var result = new BigValue(v)
      for (i <- other.v.size - 1 to 0 by -1) {
        if (v(i) >= other.v(i)) result = new BigValue(result.v.updated(i, result.v(i) - other.v(i)))
        else {
          result = new BigValue(result.v.updated(i, result.v(i) - other.v(i) + 10)).down(1, i - 1)
        }
      }
      result
    }
  }

  override def toString: String = {
    "0," + v.mkString("")
  }
}
