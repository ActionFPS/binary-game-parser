package com.actionfps.demoparser

import akka.util.ByteString
import org.scalatest.{Matchers, WordSpec}

class NewBitQueueSpec extends WordSpec with Matchers {
  "Alternate bit queue" must {
    "Produce the same result as the old bit queue" in {
      val rest = ByteString(1, 2, 3, -4, 5, 6, 7, 8, 9, 10, 11, -20)
      import Compressor._
      val oq = new BitQueue(rest)
      val nq = new BitQueue2(rest)
      oq.rembits shouldBe nq.rembits
      oq.getbits(2) shouldBe nq.getbits(2)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(3) shouldBe nq.getbits(3)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      //      println("left thing", oq.rest.toVector)
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(9) shouldBe nq.getbits(9)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest

      oq.getbits(1) shouldBe nq.getbits(1)
      oq.rembits shouldBe nq.rembits
      oq.rest shouldBe nq.rest
      oq.rest should have length 0

    }
  }

}