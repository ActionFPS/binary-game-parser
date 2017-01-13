package com.actionfps.demoparser

import java.io.File

import scala.util.{Failure, Success, Try}

object DemoToJsonApp extends App {

  def simpleSample(): Unit = {
    val a = new File( """HOME\Desktop\Demos\123512643.dmo""")
    val fopt = new File( """HOME\\acleague\\acm\\ui\\yaah.json""")
    val startOfIt = System.currentTimeMillis()
    DemoToJson.demoToJson(a, fopt)
    val endOfIt = System.currentTimeMillis()
    import concurrent.duration._
    val tookTime = (endOfIt - startOfIt).millis.toSeconds.seconds
    println(s"Took $tookTime to process the demo into json")
  }

  def processAll(): Unit = {
    val demosDir = new File("HOME\\Desktop\\Demos")
    val filesz = demosDir.listFiles.toVector.filter(_.getName.endsWith(".dmo")) //.take(20) //.take(30)
    val startTime = System.currentTimeMillis()
    val theResult = for {
      f <- filesz.toIterator.toVector.par
      name = f.getName
      of = new File(demosDir, s"$name.json.gz")
      r = Try(DemoToJson.demoToJson(f, of)).recover { case e: Throwable => throw new RuntimeException(s"Failed at game $name: $e", e) }
      o = r match {
        case Success(_) => s"Successfully generated json for $name"
        case Failure(reason) => s"Failed to generate json for $name due to $reason"
      }
      _ = println(o)
    } yield name -> o
    theResult.foreach(println)
    val endTime = System.currentTimeMillis()
    import concurrent.duration._
    val tookTime = (endTime - startTime).millis.toSeconds.seconds
    val totalSize = filesz.map(_.length()).sum
    println(s"Took $tookTime to process $totalSize bytes")
  }

  processAll()
}