package com.actionfps.demoparser.objects

import sw.ByteParser

/**
* Created by me on 06/08/2016.
*/
case class ClientDisconnected(cn: Int)

object ClientDisconnected {

  implicit val byteParser = ByteParser[ClientDisconnected] {
    case `SV_CDIS` #:: cn #:: rest =>
      (ClientDisconnected(cn), rest)
  }

  val parse = byteParser.old _
}
