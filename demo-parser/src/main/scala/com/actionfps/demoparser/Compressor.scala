package com.actionfps.demoparser

import java.util

import akka.util.ByteString
import sw.Bytes

import scala.annotation.tailrec

object Compressor {

  object ExtractString {
    def unapply(bs: ByteString) = shiftString(bs)

    def unapply(bs: Bytes) = shiftString(bs.byteString.drop(bs.position)).map { case (i, r) => i -> Bytes(r, 0) }
  }

  object ExtractInt {
    def unapply(bs: ByteString) = shiftInt(bs)

    def unapply(bs: Bytes) = shiftInt(bs.byteString.drop(bs.position)).map { case (i, r) => i -> Bytes(r, 0) }
  }

  object ExtractUInt {
    def unapply(bs: ByteString) = shiftUInt(bs)

    def unapply(bs: Bytes) = shiftUInt(bs.byteString.drop(bs.position)).map { case (i, r) => i -> Bytes(r, 0) }
  }

  object ExtractLong {
    def unapply(bs: ByteString) = shiftLong(bs)

    def unapply(bs: Bytes) = shiftLong(bs.byteString.drop(bs.position)).map { case (i, r) => i -> Bytes(r, 0) }
  }

  val #:: = ExtractInt
  val #:::: = ExtractUInt
  val #::: = ExtractLong
  val ##:: = ExtractString

  def shiftString(byteString: ByteString): Option[(String, ByteString)] = {
    @tailrec
    def go(accum: String, bytes: ByteString): Option[(String, ByteString)] = {
      shiftInt(bytes) match {
        case Some((0, rest)) => Option(accum, rest)
        case Some((n, rest)) => go(accum + n.toChar.toString, rest)
        case None => Option(accum, ByteString.empty)
      }
    }
    go("", byteString)
  }

  def shiftInt(byteString: ByteString): Option[(Int, ByteString)] = {
    byteString.headOption.collect {
      case x if x != -128 && x != -127 =>
        (x.toInt, byteString.drop(1))
      case -128 if byteString.size >= 3 =>
        val a = byteString(1)
        val b = byteString(2)
        val n = (a & 0xff) | (b << 8)
        (n, byteString.drop(3))
      case -127 if byteString.size >= 5 =>
        val a = byteString(1)
        val b = byteString(2)
        val c = byteString(3)
        val d = byteString(4)
        var n = a.toChar & 0xff
        n = n | ((b.toChar & 0xff) << 8)
        n = n | ((c.toChar & 0xff) << 16)
        n = n | ((d.toChar & 0xff) << 24)
        (n, byteString.drop(5))
    }
  }

  def shiftLong(byteString: ByteString): Option[(Long, ByteString)] = {
    byteString.headOption.collect {
      case x if x != -128 && x != -127 =>
        (x.toLong, byteString.drop(1))
      case -128 if byteString.size >= 3 =>
        val a = byteString(1)
        val b = byteString(2)
        val n = (a & 0xff).toLong | (b << 8).toLong
        (n, byteString.drop(3))
      case -127 if byteString.size >= 5 =>
        val a = byteString(1)
        val b = byteString(2)
        val c = byteString(3)
        val d = byteString(4)
        var n = (a.toChar & 0xff).toLong
        n = n | ((b.toChar & 0xff).toLong << 8)
        n = n | ((c.toChar & 0xff).toLong << 16)
        n = n | ((d.toChar & 0xff).toLong << 24)
        (n, byteString.drop(5))
    }
  }

  def intToByteString(n: Int) = {
    if (n < 128 && n > -127) ByteString(n.toByte)
    else if (n < 0x8000 && n >= -0x8000) {
      ByteString(0x80.toByte, n.toByte, (n >> 8).toByte)
    } else {
      ByteString(0x81, n.toByte, (n >> 8).toByte, (n >> 16).toByte, (n >> 24).toByte)
    }
  }

  def stringToByteString(str: String) = {
    val first = str.map(_.toInt).map(intToByteString).flatten.toArray
    val second = first ++ Array(0.toByte)
    ByteString(second)
  }

  def shiftUInt(byteString: ByteString): Option[(Int, ByteString)] = {
    val q = new CubeQueue(byteString)
    Option((q.getuint, q.rest))
  }

  class CubeQueue(var byteString: ByteString) {
    def getint = {
      val Some((v, newByteString)) = shiftInt(byteString)
      byteString = newByteString
      v
    }

    def getstring = {
      val Some((v, newByteString)) = shiftString(byteString)
      byteString = newByteString
      v
    }

    def getuint = {
      var n = getbyte.toChar & 0xff
      if ((n & 0x80) != 0) {
        n = n + ((getbyte & 0xff) << 7) - 0x80
        if ((n & (1 << 14)) != 0) {
          n = n + ((getbyte.toChar & 0xff) << 14) - (1 << 14)
        }
        if ((n & (1 << 21)) != 0) {
          n = n + ((getbyte.toChar & 0xff) << 21) - (1 << 21)
        }
        if ((n & (1 << 28)) != 0) {
          n = n | 0xF0000000
        }
      }
      n
    }

    def getbyte = {
      val firstByte = byteString.head
      byteString = byteString.tail
      firstByte
    }

    def rest = byteString
  }


  // from demo-parser

  def isBitSet(b: Byte, bit: Int) =
    (b & (1 << bit)) != 0

  implicit def byteStringToBits(byteString: ByteString): Vector[Boolean] = {
    byteString.flatMap { byte =>
      (0 until 8).map(isBitSet(byte, _))
    }.toVector
  }

  def getBitsV(bits: Vector[Boolean], n: Int): (Int, Vector[Boolean]) = {
    val (interesting, rest) = bits.splitAt(n)
    val intBits = interesting.padTo(32, false)
    var intValue = 0
    for {(true, idx) <- intBits.zipWithIndex}
      intValue = intValue | (1 << idx)
    (intValue, rest)
  }

  def putBits(stuffs: (Int, Int)*): Vector[Boolean] = {
    val allBits = for {
      (count, value) <- stuffs
      valueBits = Integer.toBinaryString(value).reverse.padTo(count, '0').map(_ == '1').reverse
    } yield valueBits
    allBits.flatten.toVector
  }

  class BitQueue2(bytes: Array[Byte]) {
    def this(byteString: ByteString) = this(byteString.toArray)

    val initialSize = bytes.length
    var rembits = 0
    val bitSet = java.util.BitSet.valueOf(bytes)
    var cursor: Int = 0

    def getbits(num: Int) = {
      var cn = 0
      var intValue = 0
      while (cn < num) {
        if (bitSet.get(cursor)) {
          intValue = intValue | (1 << cn)
        }
        cn = cn + 1
        cursor = cursor + 1
      }
      rembits = (rembits - num + 16) % 8
      intValue
    }

    def rest = {
      if (cursor >= initialSize * 8)
        ByteString.empty
      else
        ByteString(bitSet.get(cursor + (if (rembits == 0) 8 else rembits), initialSize * 8).toByteArray)
    }
  }

  class BitQueue(var bits: Vector[Boolean]) {
    val initialBitsSize = bits.size
    var tookbits = 0

    def rembitsinbyte = bits.size % 8

    def rembits = rembitsinbyte

    def getbits(n: Int) = {
      val (v, newBits) = getBitsV(bits, n)
      bits = newBits
      tookbits = tookbits + n
      //      rembits = (bits.size-tookbits) % 8
      //      rembits = (bits.size-tookbits-1) % 8
      //      if ( bits.isEmpty ) rembits = 0

      v
    }

    def rest = {
      val bs = new util.BitSet()
      val skipBits = 8 - (tookbits % 8)
      //      if ( bits.take(skipBits + 8).forall(_ == false) ) {
      //        bits.drop(skipBits + 8).zipWithIndex.foreach { case (b, i) => bs.set(i, b)}
      //      } else {
      bits.drop(skipBits).zipWithIndex.foreach { case (b, i) => bs.set(i, b) }
      //      }
      ByteString(bs.toByteArray)
    }
  }

}
