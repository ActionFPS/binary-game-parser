package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
case class SvClient(cn: Int, messages: Vector[SvClientSubMessage])

object SvClient {

  def parse(byteString: ByteString): Option[(SvClient, ByteString)] = {
    Option(byteString).collectFirst {
      case `SV_CLIENT` #:: cn #:: len #:::: datums =>
        def go(bs: ByteString, accum: Vector[SvClientSubMessage]): (Vector[SvClientSubMessage], ByteString) = {
          svMessage(bs) match {
            case Some((msg, right)) => go(right, accum :+ msg)
            case None => (accum, bs)
          }
        }
        assert(len <= datums.size, {
          s"Datums expected to be at minimum $len, has size ${datums.size}, given input $byteString"
        })
        val (svClients, rest) = datums.splitAt(len)
        val (msgs, lefties) = go(svClients, Vector.empty)

        //          def warn(assertion: Boolean, message: => Any) {
        //          if (!assertion)
        //            println("assertion failed: "+ message)
        //        }
        //
        assert(lefties.isEmpty, {
          val starts = msgs.dropRight(lefties.size)
          s"Lefties should be all eliminated, found: ${lefties.size} (from ${datums.size} & specified size was $len), $lefties - started with $starts, ended up with $msgs - original input $byteString"
        })
        (SvClient(cn, msgs), rest)
    }
  }
}
