package crypto.week1

import scala.math._

object Msg {
  def apply(text: String) = new Msg(text)

  def countHexDigits(number: BigInt) = {
    val numBytes = number.bitLength / 8
    val remainder = number.bitLength - numBytes * 8
    numBytes + (if (remainder > 0) 1 else 0)
  }

  def hex(text: String) = {
    def cvtByte(b: Byte): String = {
      (if ((b & 0xff) < 0x10) "0" else "") + BigInt(b & 0xff).toString(16)
    }
    text.getBytes().map(cvtByte(_)).mkString.toUpperCase
  }

}

case class Msg(data: BigInt) {
  import crypto.week1.Msg._

  def this(text: String) = this(BigInt(Msg.hex(text), 16))

  def apply(idx: Int) = data.toByteArray(idx)

//  def numHexDigits = countHexDigits(data)
  def size = {
    val numBytes = data.bitLength / 8
    val remainder = data.bitLength - numBytes * 8
    numBytes + (if (remainder > 0) 1 else 0)
  }

  def xor(that: Msg) = {
    val diff = (countHexDigits(data) - countHexDigits(that.data))
    val (d1, d2) =
      if (diff < 0) (data << (-diff*8), that.data)
      else          (data,              that.data << (diff*8))
    Msg(d1 ^ d2)
  }

//  def shift(n: Int) =
//    if (n >= 0) Msg1(data * pow(10, n).toInt)
//    else Msg1(data / pow(10, -n).toInt)

//  def alignTo(that: Msg1) = {
//    val diff = (countHexDigits(data) - countHexDigits(that.data))
//    if (diff < 0) {
//      Msg1(data << (-diff*8))
//    } else this
//  }

  def dropRight(n: Int) = Msg(BigInt(data.toByteArray.dropRight(n)))

  def truncate(size: Int) = {
    val diff = (countHexDigits(data) - size)
    println(s"mysize=${countHexDigits(data)}, size=$size, diff=$diff")
    if (diff > 0) {
      Msg(data >> (diff*8))
    } else this
  }

  def decode = new String(data.toByteArray)

  override def toString = {
    val text = data.toString(16)
    val missing = size - text.length
    "0" * missing + text
  }

}

//object Msg {
//  def apply(text: String) = new Msg(text)
//}
//
//case class Msg(data: Array[Int]) {
//  def this(text: String) = this(BigInt(text, 16).toByteArray.map(_.toInt))
//
//  def apply(pos: Int) = data(pos)
//
//  def decode = String.valueOf(data)
//
//  def size = data.size
//
//  def xor(that: Msg) = {
//    val x = data.zip(that.data).map(z => z._1 ^ z._2)
//    Msg(x)
//  }
//
//  def dropLeft(n: Int) = Msg(data.drop(n))
//  def dropRight(n: Int) = Msg(data.dropRight(n))
//
//  override def toString =
//    data.map(b => BigInt(b).toString(16)).map(c => if (c.size == 1) "0" + c else c).reduce(_+_)
//}
