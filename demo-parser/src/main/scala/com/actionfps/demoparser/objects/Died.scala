package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
case class Died(actor: Int, victim: Int, frags: Int, gun: Int, gib: Boolean = false)

object Died {

  def parse(byteString: ByteString): Option[(Died, ByteString)] = {
    Option(byteString).collectFirst {
      case `SV_DIED` #:: victim #:: actor #:: frags #:: gun #:: rest =>
        (Died(actor, victim, frags, gun, gib = false), rest)
      case `SV_GIBDIED` #:: victim #:: actor #:: frags #:: gun #:: rest =>
        (Died(actor, victim, frags, gun, gib = true), rest)
    }
  }
}
