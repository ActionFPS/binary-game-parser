package com.actionfps.demoparser

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import akka.util.ByteString
import com.actionfps.demoparser.DemoAnalysis.GameState
import org.json4s.NoTypeHints

object DemoToJson {

  implicit val formats = org.json4s.jackson.Serialization.formats(NoTypeHints)

  import org.json4s.jackson.Serialization.writePretty

  val headerSize = 428

  // 16 + 4 + 4 + 80 + 322

  implicit class writeToOs(val outputStream: OutputStream) extends AnyVal {
    def write(str: String) = {
      outputStream.write(str.getBytes)
    }
  }

  // very impure!
  // reduce footprint by capturing gamestate after every 30ms.
  def reduceFootprint(input: Iterator[GameState]): Iterator[GameState] = {
    var lastMillis = 0
    input.collect {
      case stt if stt.millis - lastMillis >= 50 =>
        lastMillis = stt.millis
        stt
    }
  }

  def demoToJson(inputFile: File, outputFile: File) = {
    val bs = ByteString {
      val fis = new FileInputStream(inputFile)
      try {
        val is = new GZIPInputStream(fis)
        try {
          is.skip(headerSize)
          org.apache.commons.io.IOUtils.toByteArray(is)
        } finally is.close()
      } finally fis.close()
    }
    reduceFootprint(DemoAnalysis.flowState(bs).toIterator).map(_.toJson).toStream match {
      case header #:: rest =>
        val fos = new FileOutputStream(outputFile, false)
        try {
          val fw = new GZIPOutputStream(fos, false)
          try {
            fw.write("[")
            fw.write("\n")
            fw.write(writePretty(header))
            fw.write("\n")
            rest.foreach { j =>
              fw.write(", ")
              fw.write("\n")
              fw.write(writePretty(j))
            }
            fw.write("]")
          } finally fw.close()
        } finally fos.close()
    }
  }
}