package com.actionfps.demoparser

import akka.util.ByteString
import org.scalatest.{Inspectors, Matchers, WordSpec}

class NewShiftIntSpec extends WordSpec with Matchers with Inspectors {
  def process(input: ByteString) = {
    var ha = input
    while (ha.nonEmpty) {
      val firstResult = Compressor.shiftInt(ha)
      val secondResult = firstResult
      //      val secondResult = Compressor.shiftInt2(ha)
      //      println("fr", firstResult, "sr", secondResult, "ha = ", ha, "input = ", input)
      firstResult shouldBe secondResult
      ha = firstResult.map(_._2).getOrElse(ByteString.empty)
    }
  }

  "Alternate shift int" must {
    "Produce the same result as the old shift int for the -128" in {
      Array(-128, -127, -66, -1, 0, 1, 63, 127, 128).permutations.map(k => -128 +: k).map(_.map(_.toByte)).foreach { stuff =>
        process(ByteString(stuff))
      }
    }
    "Produce the same result as the old shift int for the -127" in {
      Array(-128, -127, -66, -1, 0, 1, 63, 127, 128).permutations.map(k => -127 +: k).map(_.map(_.toByte)).foreach {
        stuff =>
          process(ByteString(stuff))
      }
    }
    "Produce the same result as the old shift int for others" in {
      Array(-128, -127, -66, -1, 0, 1, 63, 127, 128).permutations.map(_.map(_.toByte)).foreach { stuff =>
        process(ByteString(stuff))
      }
    }
  }

}