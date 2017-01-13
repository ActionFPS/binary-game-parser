package com.actionfps.demoparser.objects

import com.actionfps.demoparser.Compressor.CubeQueue

/**
* Created by me on 06/08/2016.
*/
case class PlayerInfo(cn: Int, state: Int, lifesequence: Int, primary: Int, gunselect: Int, flagscore: Int, frags: Int, deaths: Int, health: Int, armour: Int, points: Int, teamkills: Int, ammos: Vector[Int], mags: Vector[Int])

object PlayerInfo {
  def parse(byteString: ByteString) = {
    Option(byteString).collectFirst {
      case co #:: state #:: lifesequence #:: primary #:: gunselect #:: flagscore #:: frags #:: deaths #:: health #:: armour #:: points #:: teamkills #:: rest =>
        val q = new CubeQueue(rest)
        val ammos = (1 to 10).map(_ => q.getint)
        val mags = (1 to 10).map(_ => q.getint)
        val pi = PlayerInfo(co, state, lifesequence, primary, gunselect, flagscore, frags, deaths, health, armour, points, teamkills, ammos.toVector, mags.toVector)
        (pi, q.rest)
    }
  }
}
