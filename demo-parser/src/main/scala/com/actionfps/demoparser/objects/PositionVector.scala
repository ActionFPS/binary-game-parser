package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
object PositionVector {
  def empty = PositionVector()
}

case class PositionVector(x: Float = 0.0f, y: Float = 0.0f, z: Float = 0.0f)
