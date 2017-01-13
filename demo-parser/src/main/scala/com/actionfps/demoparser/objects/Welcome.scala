package com.actionfps.demoparser.objects

import sw.ByteParser

/**
  * Created by me on 06/08/2016.
  */
case class Welcome(numClients: Int, mapChange: MapChange, timeUp: TimeUp, il: ItemList, resume: Resume,
                   serverMode: Int, motd: Option[String])

object Welcome {

  val byteParser = ByteParser[Welcome] {
    case `SV_WELCOME` #::
      numclients #::
      MapChange.byteParser(mc,
      TimeUp.byteParser(tu,
      ItemList.byteParser(il, rest34))) =>
      val resumer = Resume.byteParser(numclients)
      rest34 match {
        case resumer(re, `SV_SERVERMODE` #:: smode #:: rest5) =>
          rest5 match {
            case `SV_TEXT` #:: txt ##:: moar =>
              (Welcome(numclients, mc, tu, il, re, smode, Some(txt)), moar)
            case other =>
              (Welcome(numclients, mc, tu, il, re, smode, None), other)
          }
      }
  }

  val parse = byteParser.old _

}
