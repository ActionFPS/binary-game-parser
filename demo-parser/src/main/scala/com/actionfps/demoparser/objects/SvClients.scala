package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
case class SvClients(svClients: Vector[SvClient])

object SvClients {

  def parse(byteString: ByteString): Option[(SvClients, ByteString)] = {
    def go(bs: ByteString, accum: Vector[SvClient]): (Vector[SvClient], ByteString) = {
      SvClient.parse(bs) match {
        case Some((c, r)) => go(r, accum :+ c)
        case None => (accum, bs)
      }
    }
    Option(byteString).collectFirst {
      case `SV_CLIENT` #:: _ =>
        val (svClients, rest) = go(byteString, Vector.empty)
        //          assert(rest.isEmpty, "svClients should have been eliminated, but still have: " + rest)
        (SvClients(svClients), rest)
    }
  }
}
