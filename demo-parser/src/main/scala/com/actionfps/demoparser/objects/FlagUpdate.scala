package com.actionfps.demoparser.objects

import com.actionfps.demoparser.Compressor
import com.actionfps.demoparser.Compressor.CubeQueue

/**
* Created by me on 06/08/2016.
*/
object FlagUpdate {

  def parse(byteString: ByteString) = {
    Option(byteString).collectFirst {
      case `SV_FLAGINFO` #:: flag #:: state #:: rest =>
        if (state == 1) {
          val Some((cn, rest2)) = Compressor.shiftInt(rest)
          (FlagUpdate(flag, state, Option(cn), None), rest2)
        } else if (state == 2) {
          val q = new CubeQueue(rest)
          (FlagUpdate(flag, state, None, Option(PositionVector(q.getuint / DMF, q.getuint / DMF, q.getuint * DMF))), q.rest)
        } else {
          (FlagUpdate(flag, state, None, None), rest)
        }
    }
  }
}

case class FlagUpdate(num: Int, newState: Int, carrying: Option[Int], dropped: Option[PositionVector]) {
  def inBase = newState == 0

  def stolen = newState == 1

  def wasDropped = newState == 2

  def idle = newState == 3
}
