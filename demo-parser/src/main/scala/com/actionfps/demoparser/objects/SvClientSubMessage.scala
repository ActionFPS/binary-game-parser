package com.actionfps.demoparser.objects

/**
  * Created by me on 06/08/2016.
  */
sealed trait SvClientSubMessage

object SvClientSubMessage {

  object Ignore {
    def apply(n: Int): Ignore = Ignore(symbols(n))
  }

  case class Ignore(sv: Symbol) extends SvClientSubMessage

  case class SvWelcome(welcome: Welcome) extends SvClientSubMessage

  case class WeaponChange(weapon: Int) extends SvClientSubMessage


  case class SwitchName(newName: String) extends SvClientSubMessage

  case class SwitchTeam(newTeam: Int) extends SvClientSubMessage

  case class SvText(value: String, me: Boolean = false) extends SvClientSubMessage

  case class SvSpawn(lifeSequence: Int, health: Int, armour: Int, gun: Int, ammos: Vector[Int], mags: Vector[Int]) extends SvClientSubMessage

  case class SvSetteam(cn: Int, team: Int) extends SvClientSubMessage

  case class SvSound(soundId: Int) extends SvClientSubMessage

}
