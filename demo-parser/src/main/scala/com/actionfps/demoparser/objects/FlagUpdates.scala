package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
object FlagUpdates {

  def parse(byteString: ByteString) = {
    Option(byteString).collectFirst {
      case `SV_FLAGINFO` #:: _ =>
        def go(bs: ByteString, accum: Vector[FlagUpdate]): (Vector[FlagUpdate], ByteString) = {
          FlagUpdate.parse(bs) match {
            case Some((fi, o)) => go(o, accum :+ fi)
            case None => (accum, bs)
          }
        }
        val (fis, rest) = go(byteString, Vector.empty)
        if (rest.nonEmpty) {
          println("After SV_FLAGINFO", rest)
        }
        (FlagUpdates(fis), rest)
    }
  }
}

case class FlagUpdates(items: Vector[FlagUpdate])
