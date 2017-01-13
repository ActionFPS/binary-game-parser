package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
case class PlayerEnt(lifesequence: Int, o: PositionVector, vel: PositionVector, eyeheight: Float, onfloor: Boolean, scoping: Boolean, yaw: Float, pitch: Float, move: Int, roll: Float, strafe: Float, onladder: Boolean, lastpos: Int)
