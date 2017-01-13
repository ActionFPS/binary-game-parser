package com.actionfps.demoparser

import java.io.{File, FileInputStream, FileWriter}
import java.util.zip.GZIPInputStream

import akka.util.ByteString
import org.json4s.NoTypeHints

import scala.language.implicitConversions
import scala.util.Try

/**
  * Created by William on 14/02/2015.
  */
object DPB {

  def parseFile(file: File) = {
    val is = new GZIPInputStream(new FileInputStream(file))
    try {
      (1 to 428).foreach(_ => is.read())
      val bs = org.apache.commons.io.IOUtils.toByteArray(is)
      DemoAnalysis.goThroughPackets(ByteString(bs))
    } finally is.close()
  }
  def parseFile2(file: File) = {
    val is = new GZIPInputStream(new FileInputStream(file))
    try {
      (1 to 428).foreach(_ => is.read())
      val bs = org.apache.commons.io.IOUtils.toByteArray(is)
      DemoAnalysis.goThroughPackets2(ByteString(bs))
    } finally is.close()
  }

}
object DemoParserApp extends App {
  import DPB._
  // 1358364864 just fails, why?
  //  Set(74).map(DemoParser.symbols).foreach(println)
  //    println(DemoParser.symbols(107))
  //  println(DemoParser.symbols(73))
  //  println(DemoParser.symbols.indexOf('SV_SOUND))

  //  println(SvClients.parse(ByteString(88, 1, 12, 12, 98, 6, 65, 119, 101, 115, 111, 109, 101, 33, 0, 88, 2, 2, 11, 58)))
  //  println(SvClients.parse(ByteString(88, 7, -127, 1, 47, -128, 107, 11, 47, -128, 15, 20, 47, -128, -3, 22, 47, -128, 60, 25, 47, -128, -56, 26, 47, -128, -33, 27, 47, -128, -98, 28, 47, -128, 19, 29, 47, -128, 75, 29, 47, -128, 29, 29, 47, -128, -51, 28, 47, -128, 89, 28, 47, -128, -50, 27, 47, -128, 3, 27, 47, -128, 48, 26, 47, -128, 79, 25, 47, -128, 47, 24, 47, -128, 21, 23, 47, -128, -8, 21, 47, -128, -115, 20, 47, -128, 44, 19, 47, -128, 119, 17, 47, -128, -39, 15, 47, -128, 78, 14, 47, -128, 127, 12, 47, -128, -45, 10, 34, 13, 100, 0, 6, 1, 50, 0, 0, 0, 0, 40, 0, 0, 0, 1, 10, 0, 0, 0, 0, 20, 0, 0, 0)))
  //  println()
  //  println(SvClients.parse(ByteString(88, 7, -127, 1, 47, -128, 107, 11, 47, -128, 15, 20, 47, -128, -3, 22, 47, -128, 60, 25, 47, -128, -56, 26, 47, -128, -33, 27, 47, -128, -98, 28, 47, -128, 19, 29, 47, -128, 75, 29, 47, -128, 29, 29, 47, -128, -51, 28, 47, -128, 89, 28, 47, -128, -50, 27, 47, -128, 3, 27, 47, -128, 48, 26, 47, -128, 79, 25, 47, -128, 47, 24, 47, -128, 21, 23, 47, -128, -8, 21, 47, -128, -115, 20, 47, -128, 44, 19, 47, -128, 119, 17, 47, -128, -39, 15, 47, -128, 78, 14, 47, -128, 127, 12, 47, -128, -45, 10, 34, 13, 100, 0, 6, 1, 50, 0, 0, 0, 0, 40, 0, 0, 0, 1, 10, 0, 0, 0, 0, 20, 0, 0, 0)))
  //  println(SvClients.parse(ByteString(88, 1, -128, 1, 47, -128, 85, 8, 47, -128, 46, 13, 47, -128, 10, 17, 47, -128, -28, 19, 47, -128, 22, 22, 47, -128, -115, 23, 47, -128, -106, 24, 47, -128, 69, 25, 47, -128, -88, 25, 47, -128, -52, 25, 47, -128, -116, 25, 47, -128, 40, 25, 47, -128, -90, 24, 47, -128, 11, 24, 47, -128, 91, 23, 47, -128, -102, 22, 47, -128, -54, 21, 47, -128, -18, 20, 47, -128, 8, 20, 47, -128, 26, 19, 47, -128, 37, 18, 47, -128, 42, 17, 47, -128, 42, 16, 47, -128, 38, 15, 47, -128, 31, 14, 47, -128, 21, 13, 47, -128, 9, 12, 47, -128, -5, 10, 47, -128, -21, 9, 47, -128, -38, 8, 47, -128, -56, 7, 47, -128, -75, 6, 88, 3, 4, 47, -128, 4, 1)))
  //  println(SvClients.parse(ByteString(88, 7, -127, 1, 47, -128, 107, 11, 47, -128, 15, 20, 47, -128, -3, 22, 47, -128, 60, 25, 47, -128, -56, 26, 47, -128, -33, 27, 47, -128, -98, 28, 47, -128, 19, 29, 47, -128, 75, 29, 47, -128, 29, 29, 47, -128, -51, 28, 47, -128, 89, 28, 47, -128, -50, 27, 47, -128, 3, 27, 47, -128, 48, 26, 47, -128, 79, 25, 47, -128, 47, 24, 47, -128, 21, 23, 47, -128, -8, 21, 47, -128, -115, 20, 47, -128, 44, 19, 47, -128, 119, 17, 47, -128, -39, 15, 47, -128, 78, 14, 47, -128, 127, 12, 47, -128, -45, 10, 34, 13, 100, 0, 6, 1, 50, 0, 0, 0, 0, 40, 0, 0, 0, 1, 10, 0, 0, 0, 0, 20, 0, 0, 0)))
  //  System.exit(0)

  import org.json4s.jackson.Serialization.write

  val demosDir = new File("HOME\\Desktop\\Demos")
  //  val failedOnes = Vector(1003986242,1043319913,1274560658,1211763285,1697579163,1955177189,1983365056,
  //  1985324506,2066960023,916451708,1615509721,1336408268,1095211318,241713795)
  //  val failedOnes = Vector(1003986242,1043319913,1274560658,1211763285,1697579163,1955177189,1983365056,
  //  1985324506,2066960023,916451708,1615509721,1336408268,1095211318,241713795)
  //  val failedOnes = Vector(916451708,1615509721, 1274560658, 241713795)
  // fails: 1955177189
  var currentGame: String = ""
  objects.report = (f) => {
    println("Game id", currentGame, "Zeroed Position result", f)
  }
  val filesz = demosDir.listFiles.toVector
  //.take(30)
  val startTime = System.currentTimeMillis()
  val theResult = for {
    f <- filesz.toIterator.toVector.par
    name = f.getName

    //    if name != "1955177189.dmo"
    //if name == "1415925714.dmo"
    //    if failedOnes.exists(f => name.contains(f.toString))
    if name.endsWith(".dmo")
    cg = {
      currentGame = name
    }
    pr = {
      println("Processing", name)
      val r = Try(parseFile(f).size).recover { case e: Throwable => throw new RuntimeException(s"Failed at game $name: $e", e) }
      println("Finished processing", name, "got result: ", r)
      r
    }
  } yield name -> pr
  //  theResult.map(_._2.get).foreach(println)

  theResult.filter(_._2.isFailure).foreach(println)
  //  theResult.map(_._2.get).foreach(println)

  val endTime = System.currentTimeMillis()

  import concurrent.duration._

  val tookTime = (endTime - startTime).millis.toSeconds.seconds
  val totalSize = filesz.map(_.length()).sum
  println(s"Took $tookTime to process $totalSize bytes")
  System.exit(0)
  implicit val formats = org.json4s.jackson.Serialization.formats(NoTypeHints)

  //  println(DemoParser.symbols(88))
  //  println(DemoParser.symbols(73))
  //  println(SvClients.parse(ByteString(88, 5, 3, 70, 3, 0)))
  //
  //  System.exit(0)
  //  go(bs).take(50).flatMap(e => initClient(e.data)).foreach(println)
  //  go(bs).take(1).foreach(println)
  //  go(bs).take(1).flatMap(e => welkom(e.data)).foreach(println)

  //  val ff = new File("""HOME\Downloads\752376438.dmo""")
  //  val ff = new File("""HOME\Downloads\112886765.dmo""")
  //  val ff = new File("""HOME\Downloads\737537936.dmo""")
  //val ff = new File("""HOME\Downloads\1889875423.dmo""")
  val ff = new File("""HOME\Downloads\123512643.dmo""")
  //  val ff = new File("""HOME\Downloads\1600624985.dmo""")
  //  val headerSize = 16 + 4 + 4 + 80 + 322
  val headerSize = 428
  lazy val bs = ByteString {
    val is = new GZIPInputStream(new FileInputStream(ff))
    org.apache.commons.io.IOUtils.toByteArray(is).toStream.drop(428).toArray
    //    Stream.continually(is.read()).takeWhile(-1 != _).map(_.toByte).toArray.drop(428) //.take(5000)
  }

  //  go(bs).flatMap(e => PositionResults.parse(e.data)).foreach(println)


  //  val initialState = go(bs).flatMap(bs => Welcome.parse(bs.data)).head._1 : GameState

  //  stateFlow take 50 foreach println
  //  stateFlow take 50 map (_.toJson) foreach println

  //  val jsons = stateFlow.grouped(10).map(_.head).take(50).map(_.toJson)

  //  stateFlow.drop(200).take(1).map(_.toJson).foreach(println)
  //  System.exit(0)
  //  val jsonss = stateFlow.take(50).map(_.toJson).foreach(println)
  val jsons = DemoAnalysis.flowState(bs).map(_.toJson)
  //  val jsons = stateFlow.drop(1500).take(1500).map(_.toJson)
  //  val out = write(jsons.toVector)
  val fw = new FileWriter("""HOME\\acm\\ui\\yaah.json""")
  fw.write("[")
  fw.write(write(jsons.head))
  jsons.tail.foreach { j =>
    fw.write(", ")
    fw.write(write(j))
  }
  fw.write("]")
  fw.close()

}
