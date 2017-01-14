package com.actionfps.demoparser

import java.io.FileInputStream

import akka.util.ByteString
import com.actionfps.demoparser.objects.DemoPacketHeader

/**
  * Created by me on 14/01/2017.
  */
object RawParseApp extends App {
  val file = new FileInputStream("/Users/me/live_1999.dmo")
  val headers = Array.fill(12)(0.toByte)
  val rest = Array.fill(2048)(0.toByte)

  def hbs = ByteString(headers)

  while (true) {
    file.read(headers)
    val DemoPacketHeader(h) = hbs
    file.read(rest, 0, h.length)
    val dta = ByteString(rest).take(h.length)
    com.actionfps.demoparser.DemoAnalysis.extractBasicsz.lift.apply(dta) match {
      case Some(oh) => //println(s"${h} Good: ${oh}")
      case None =>
        val sym = objects.symbols.lift(dta(0))
        if (s"$sym".contains("TEXT")) println(s"${h} Ignored: ${dta}")
    }
  }
}
