package com.actionfps.demoparser.objects

/**
  * Created by me on 06/08/2016.
  */
object FlagCount {

  def parse(byteString: ByteString) = {
    Option(byteString).collectFirst {
      case `SV_FLAGCNT` #:: cn #:: flags #:: rest =>
        (FlagCount(cn, flags), rest)
    }
  }
}

/**
  * but shots are sent via SV_SHOTFX
  * if you want to make accuracy stats yes otherwise no.
  * if you want to track players health you would need to handle SV_DAMAGE
  * and SV_GIBDAMAGE
  */

case class FlagCount(cn: Int, flags: Int)
