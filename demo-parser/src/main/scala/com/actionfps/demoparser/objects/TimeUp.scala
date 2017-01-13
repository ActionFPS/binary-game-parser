package com.actionfps.demoparser.objects

import sw.ByteParser

/**
  * Created by me on 06/08/2016.
  */
case class TimeUp(millis: Int, limit: Int)

object TimeUp {

  val byteParser = ByteParser[TimeUp] {
    case `SV_TIMEUP` #:: millis #:: gamelimit #:: rest =>
      (TimeUp(millis, gamelimit), rest)
  }

}
