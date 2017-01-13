package com.actionfps.demoparser.objects

import scala.util.control.NonFatal

/**
* Created by me on 06/08/2016.
*/
object PositionResult {
  def parse(input: ByteString) =
    try {
      parsePosition(input)
    } catch {
      case NonFatal(e) => throw new RuntimeException(s"Failed to parse position due to: $e. Data: $input", e)
    }
}

case class PositionResult(cn: Int, f: Int, pos: PositionVector, vel: PositionVector, yaw: Float, pitch: Float, roll: Float, scoping: Boolean, compressed: Boolean, shooting: Boolean)
