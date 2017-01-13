package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
object InitClient {

  def parse(byteString: ByteString) = {
    Option(byteString).collectFirst {
      case `SV_INITCLIENT` #:: cn #:: name ##:: claSkin #:: rvsfSkin #:: team #:: ip #:: rest =>
        (InitClient(cn, name, claSkin, rvsfSkin, team, ip), rest)
    }
  }
}

case class InitClient(cn: Int, name: String, claSkin: Int, rvsfSkin: Int, team: Int, ip: Int)
