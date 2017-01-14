package com.actionfps.demoparser

import java.io.{FileInputStream, RandomAccessFile}

import akka.util.ByteString
import com.actionfps.demoparser.objects.Welcome
import org.scalatest.{FreeSpec, Matchers}

/**
  * Created by me on 14/01/2017.
  */
class FigurePositions extends FreeSpec with Matchers {

  case class MessageHeader(millis: Int, chan: Int, length: Int)

  object MessageHeader {
    def unapply(byteString: ByteString): Option[MessageHeader] = {
      if (byteString.length < 12) None else {
        Some {
          val buffer = byteString.asByteBuffer
          MessageHeader(
            java.lang.Integer.reverseBytes(buffer.getInt),
            java.lang.Integer.reverseBytes(buffer.getInt),
            java.lang.Integer.reverseBytes(buffer.getInt)
          )
        }
      }
    }
  }

  "It worksszz" in {
    val randomAccessFile = new RandomAccessFile("/Users/me/live_1999.dmo", "r")

    def test_position(): Boolean = {
      if (!(randomAccessFile.read() == 1)) return false
      randomAccessFile.read()
      if (!(randomAccessFile.read() == 42)) return false
      if (!(randomAccessFile.read() == 97)) return false
      if (!(randomAccessFile.read() == 99)) return false
      randomAccessFile.read() == 95
    }

    val MIN_LENGTH = 200
    try {
      var position = randomAccessFile.length() - MIN_LENGTH
      var found = false
      while (!found && position > 0) {
        randomAccessFile.seek(position)
        if (test_position()) found = true else {
          position = position - 1
        }
      }
      randomAccessFile.seek(position)
      val welcomeTgt = Array.fill(2048)(0.toByte)
      randomAccessFile.read(welcomeTgt)
      val bs = ByteString(welcomeTgt)
      println(Welcome.parse(bs).map(_._1))
      println(s"Found? ${found}, position: ${position}")
    } finally randomAccessFile.close()
  }
  "It works" ignore {
    val fis = new FileInputStream("/Users/me/live_1999.dmo")
    try {
      val headerTgt = Array.fill(12)(0.toByte)
      var num = 100000
      val rest = Array.fill(2048)(0.toByte)
      while (num > 0) {
        fis.read(headerTgt)
        val bs = ByteString(headerTgt)
        val MessageHeader(h) = bs
        fis.read(rest, 0, h.length)
        val restPos = Welcome.parse(ByteString(rest))
        val welcomeDefined = restPos.isDefined
        val matchingPattern = rest(0) == 1 && rest(2) == 42 && rest(3) == 97 && rest(4) == 99 && rest(5) == 95
        if (welcomeDefined || matchingPattern) {
          println(welcomeDefined, matchingPattern)
        }
        //        if ( restPos.isDefined ) println(ByteString(rest.take(200)))
        //        restPos.map(_._1).filter(_.timeUp.millis != 0).foreach(println)
        //        println(h)
        num = num - 1
      }
      //    try {
      //      val tgtArr = Array.fill(2048)(0.toByte)
      //      Iterator.continually(fis.read()).take(12).foreach(identity)
      //      fis.read(tgtArr)
      //      println(tgtArr.take(12).toList)
      //      println(Welcome.parse(bs.drop(12)).map(_._1))
      //      println(objects.symbols(98))
      //      println(objects.SV_WELCOME)
      println(objects.SV_MAPCHANGE)
    } finally fis.close()
  }
}
