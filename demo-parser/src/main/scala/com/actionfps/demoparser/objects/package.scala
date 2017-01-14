package com.actionfps.demoparser

import akka.util.ByteString
import com.actionfps.demoparser.Compressor.{BitQueue2, CubeQueue, ExtractInt, ExtractLong, ExtractString, ExtractUInt}
import com.actionfps.demoparser.objects.SvClientSubMessage._

/**
  * Created by me on 06/08/2016.
  */
package object objects {

  val DMF = 16.0f
  val DNF = 100.0f
  val DVELF = 4.0f

  val voteSymbols = List(
    'SA_KICK, 'SA_BAN, 'SA_REMBANS, 'SA_MASTERMODE, 'SA_AUTOTEAM, 'SA_FORCETEAM, 'SA_GIVEADMIN, 'SA_MAP, 'SA_RECORDDEMO, 'SA_STOPDEMO, 'SA_CLEARDEMOS, 'SA_SERVERDESC, 'SA_SHUFFLETEAMS, 'SA_SWITCHTEAMS, 'SA_SORTTEAMS, 'SA_PAUSEGAME, 'SA_RESUMEGAME, 'SA_AUTOPAUSE, 'SA_AUTORESUME, 'SA_HALFTIME, 'SA_NUM
  )
  val symbols = List(
    'SV_SERVINFO, 'SV_WELCOME, 'SV_INITCLIENT, 'SV_POS, 'SV_POSC, 'SV_POSN, 'SV_TEXT, 'SV_TEAMTEXT, 'SV_TEXTME, 'SV_TEAMTEXTME, 'SV_TEXTPRIVATE,
    'SV_SOUND, 'SV_VOICECOM, 'SV_VOICECOMTEAM, 'SV_CDIS,
    'SV_SHOOT, 'SV_EXPLODE, 'SV_SUICIDE, 'SV_AKIMBO, 'SV_RELOAD, 'SV_AUTHT, 'SV_AUTHREQ, 'SV_AUTHTRY, 'SV_AUTHANS, 'SV_AUTHCHAL,
    'SV_GIBDIED, 'SV_DIED, 'SV_GIBDAMAGE, 'SV_DAMAGE, 'SV_HITPUSH, 'SV_SHOTFX, 'SV_THROWNADE,
    'SV_TRYSPAWN, 'SV_SPAWNSTATE, 'SV_SPAWN, 'SV_SPAWNDENY, 'SV_FORCEDEATH, 'SV_RESUME,
    'SV_DISCSCORES, 'SV_TIMEUP, 'SV_EDITENT, 'SV_ITEMACC,
    'SV_MAPCHANGE, 'SV_ITEMSPAWN, 'SV_ITEMPICKUP,
    'SV_PING, 'SV_PONG, 'SV_CLIENTPING, 'SV_GAMEMODE,
    'SV_EDITMODE, 'SV_EDITH, 'SV_EDITT, 'SV_EDITS, 'SV_EDITD, 'SV_EDITE, 'SV_NEWMAP,
    'SV_SENDMAP, 'SV_RECVMAP, 'SV_REMOVEMAP,
    'SV_SERVMSG, 'SV_ITEMLIST, 'SV_WEAPCHANGE, 'SV_PRIMARYWEAP,
    'SV_FLAGACTION, 'SV_FLAGINFO, 'SV_FLAGMSG, 'SV_FLAGCNT,
    'SV_ARENAWIN,
    'SV_SETADMIN, 'SV_SERVOPINFO,
    'SV_CALLVOTE, 'SV_CALLVOTESUC, 'SV_CALLVOTEERR, 'SV_VOTE, 'SV_VOTERESULT,
    'SV_SETTEAM, 'SV_TEAMDENY, 'SV_SERVERMODE,
    'SV_IPLIST,
    'SV_LISTDEMOS, 'SV_SENDDEMOLIST, 'SV_GETDEMO, 'SV_SENDDEMO, 'SV_DEMOPLAYBACK,
    'SV_CONNECT,
    'SV_SWITCHNAME, 'SV_SWITCHSKIN, 'SV_SWITCHTEAM,
    'SV_CLIENT,
    'SV_EXTENSION,
    'SV_MAPIDENT, 'SV_HUDEXTRAS, 'SV_POINTS,
    'SV_NUM
  )

  val SV_POS = symbols.indexOf('SV_POS)
  val SV_POSC = symbols.indexOf('SV_POSC)


  def parsePosition(input: ByteString): Option[(PositionResult, ByteString)] = {
    if (input.isEmpty) None
    else if (input(0) == SV_POSC) {
      val rest = input.drop(1)
      Option {
        val q = new BitQueue2(rest.toArray)
        //        val q = new BitQueue(rest)
        val cn = q.getbits(5)
        val usefactor = q.getbits(2) + 7
        var o = PositionVector(
          x = q.getbits(usefactor + 4) / DMF,
          y = q.getbits(usefactor + 4) / DMF
        )
        val yaw = q.getbits(9) * 360.0f / 512
        val pitch = (q.getbits(8) - 128) * 90.0f / 127
        val roll = if (q.getbits(1) == 1) 0.0f else ((q.getbits(6) - 32) * 20.0f / 31)
        val vel = if (q.getbits(1) == 1) PositionVector.empty
        else PositionVector(
          x = (q.getbits(4) - 8) / DVELF,
          y = (q.getbits(4) - 8) / DVELF,
          z = (q.getbits(4) - 8).toFloat / DVELF
        )
        val f = q.getbits(8)
        val negz = q.getbits(1) == 1
        val full = q.getbits(1) == 1
        var s = q.rembits
        if (s < 3) s = s + 8
        if (full) s = 11
        var z = q.getbits(s)
        if (negz) z = 0 - z
        o = o.copy(z = z)
        val scoping = q.getbits(1) == 1
        val shooting = q.getbits(1)
        // g appears to not be set
        (PositionResult(cn, f, o, vel, yaw, pitch, roll, scoping, compressed = true, shooting = shooting == 1), q.rest)
      }
    } else if (input(0) == SV_POS) {
      val rest = input.drop(1)
      Option {
        val q = new CubeQueue(rest)
        import q._
        val cn = getint
        val o = PositionVector(
          x = getuint / DMF,
          y = getuint / DMF,
          z = getuint / DMF
        )
        val yaw = getuint.toFloat // 0
        val pitch = getint.toFloat // 0
        val g = getuint // 16
        var roll = 0.0f
        if (((g >> 3) & 1) != 0) roll = (getint * 20.0f / 125.0f)
        val vel = PositionVector(
          x = if ((g & 1) == 1) {
            getint / DVELF
          } else 0,
          y = if (((g >> 1) & 1) == 1) {
            getint / DVELF
          } else 0,
          z = if (((g >> 2) & 1) == 1) {
            getint / DVELF
          } else 0
        )
        val scoping = ((g >> 4) & 1) == 1
        var wasEmpty = false
        val f = if (q.rest.nonEmpty) getuint else 0
        (PositionResult(cn, f, o, vel, yaw, pitch, roll, scoping, compressed = false, shooting = false), q.rest)
      }
    } else None
  }

  var report: PositionResult => Unit = x => ()



  def combine(playerEnt: PlayerEnt, positionResult: PositionResult): PlayerEnt = {
    // : PlayerEnt = {
    var f = positionResult.f
    val seqcolor = (f >> 6) & 1
    if (seqcolor != (playerEnt.lifesequence & 1)) return playerEnt
    var newVel = playerEnt.vel

    {
      val dr = positionResult.pos.x - playerEnt.o.x
      if (dr == 0) newVel = newVel.copy(x = 0.0f)
      else if (newVel.x != 0) newVel = newVel.copy(x = dr * 0.05f + 0.95f * playerEnt.vel.x)
      newVel = newVel.copy(x = newVel.x + positionResult.vel.x)
    }

    {
      val dr = positionResult.pos.y - playerEnt.o.y
      if (dr == 0) newVel = newVel.copy(y = 0.0f)
      else if (newVel.y != 0) newVel = newVel.copy(y = dr * 0.05f + 0.95f * playerEnt.vel.y)
      newVel = newVel.copy(y = newVel.y + positionResult.vel.y)
    }

    {
      val dr = positionResult.pos.z - playerEnt.o.z + playerEnt.eyeheight
      if (dr == 0) newVel = newVel.copy(z = 0.0f)
      else if (playerEnt.vel.z != 0) newVel = newVel.copy(z = dr * 0.05f + 0.95f * playerEnt.vel.z)
      newVel = newVel.copy(z = newVel.z + positionResult.vel.z)
      if (playerEnt.onfloor && newVel.z < 0) newVel = newVel.copy(z = 0)
    }

    val newPos = positionResult.pos.copy(positionResult.pos.z + playerEnt.eyeheight)
    val newYaw = positionResult.yaw
    val newPitch = positionResult.pitch
    val newStrafe = if ((f & 3) == 3) -1 else (f & 3)
    f = f >> 2
    val newMove = if ((f & 3) == 3) -1 else (f & 3)
    f = f >> 2
    val newOnFloor = (f & 1) != 0
    f = f >> 1
    val newOnLadder = (f & 1) != 0
    f = f >> 2
    playerEnt.copy(o = newPos, yaw = newYaw, pitch = newPitch, strafe = newStrafe, move = newMove, onfloor = newOnFloor, onladder = newOnLadder)
  }

  //  val bs = ByteString(scala.io.Source.fromFile(ff)(scala.io.Codec.ISO8859).map(_.toByte).take(50000).toArray)//.drop(headerSize).take(5000).toArray)
  //  println(bs)

  def extractDemoStuff(stuff: ByteString): Option[(DemoPacket, ByteString)] = {
    if (stuff.isEmpty) return None
    if (stuff.length < 12) return None
    val (header, rest) = stuff.splitAt(12)
    val (millis, chan, len) = {
      val buffer = header.asByteBuffer
      (java.lang.Integer.reverseBytes(buffer.getInt),
        java.lang.Integer.reverseBytes(buffer.getInt),
        java.lang.Integer.reverseBytes(buffer.getInt))
    }
    val (packetData, other) = rest.splitAt(len)
    Option((DemoPacket(millis, chan, packetData), other))
  }


  def demoPacketsStream(start: ByteString): Stream[DemoPacket] = {
    extractDemoStuff(start) match {
      case Some((ds, other)) => ds #:: demoPacketsStream(other)
      case None => Stream.empty
    }
  }
  //      val SV_FLAGACTION= DemoParser.symbols.indexOf('SV_FLAGACTION)
  //      val SV_FLAGMSG = DemoParser.symbols.indexOf('SV_FLAGMSG)
  //      val SV_FLAGACTION= DemoParser.symbols.indexOf('SV_FLAGACTION)

  //      val SV_FLAGMSG = DemoParser.symbols.indexOf('SV_FLAGMSG)


  val SV_FLAGINFO = symbols.indexOf('SV_FLAGINFO)

  val SV_INITCLIENT = symbols.indexOf('SV_INITCLIENT)

  val SV_CLIENT = symbols.indexOf('SV_CLIENT)

  val SV_SOUND = symbols.indexOf('SV_SOUND)

  val SV_VOICECOMTEAM = symbols.indexOf('SV_VOICECOMTEAM)

  val SV_VOICECOM = symbols.indexOf('SV_VOICECOM)

  val SV_TEXT = symbols.indexOf('SV_TEXT)

  val SV_TEXTME = symbols.indexOf('SV_TEXTME)

  val SV_SWITCHNAME = symbols.indexOf('SV_SWITCHNAME)

  val SV_SWITCHTEAM = symbols.indexOf('SV_SWITCHTEAM)

  val SV_SWITCHSKIN = symbols.indexOf('SV_SWITCHSKIN)

  val SV_EDITMODE = symbols.indexOf('SV_EDITMODE)

  val SV_SPAWN = symbols.indexOf('SV_SPAWN)

  val SV_THROWNADE = symbols.indexOf('SV_THROWNADE)

  val SV_NEWMAP = symbols.indexOf('SV_NEWMAP)

  val SV_CLIENTPING = symbols.indexOf('SV_CLIENTPING)

  val SV_WEAPCHANGE = symbols.indexOf('SV_WEAPCHANGE)

  val SV_MAPIDENT = symbols.indexOf('SV_MAPIDENT)

  val SV_WELCOME = symbols.indexOf('SV_WELCOME)

  val SV_SETTEAM = symbols.indexOf('SV_SETTEAM)

  val SV_CALLVOTE = symbols.indexOf('SV_CALLVOTE)

  val SV_VOTE = symbols.indexOf('SV_VOTE)

  val SV_FLAGMSG = symbols.indexOf('SV_FLAGMSG)

  val SV_IPLIST = symbols.indexOf('SV_IPLIST)

  val SV_VOTERESULT = symbols.indexOf('SV_VOTERESULT)

  val SV_GAMEMODE = symbols.indexOf('SV_GAMEMODE)

  //    val SV_WELCOME = symbols.indexOf('SV_WELCOME)

  val SV_RESUME = symbols.indexOf('SV_RESUME)

  val SV_GIBDIED = symbols.indexOf('SV_GIBDIED)

  val SV_DIED = symbols.indexOf('SV_DIED)

  val SV_CDIS = symbols.indexOf('SV_CDIS)

  val SV_SERVERMODE = symbols.indexOf('SV_SERVERMODE)

  val SV_MAPCHANGE = symbols.indexOf('SV_MAPCHANGE)

  val SV_FLAGCNT = symbols.indexOf('SV_FLAGCNT)

  type ByteString = akka.util.ByteString
  val ByteString = akka.util.ByteString

  val #:: = ExtractInt
  val #:::: = ExtractUInt
  val #::: = ExtractLong
  val ##:: = ExtractString

  val SV_TIMEUP = symbols.indexOf('SV_TIMEUP)

  val SV_ITEMLIST = symbols.indexOf('SV_ITEMLIST)

  def svMessage(byteString: ByteString): Option[(SvClientSubMessage, ByteString)] = {
    import SvMessageSymbols._
    Option(byteString).collectFirst {
      case `SV_SOUND` #:: _ #:: rest => (Ignore(SV_SOUND), rest)
      case `SV_WELCOME` #:: _ =>
        val Some((welcum, rest)) = Welcome.parse(byteString)
        (SvWelcome(welcum), rest)
      case `SV_VOICECOMTEAM` #:: _ #:: _ #:: rest => (Ignore(SV_VOICECOMTEAM), rest)
      case `SV_VOICECOM` #:: _ #:: rest => (Ignore(SV_VOICECOM), rest)
      case `SV_TEXTME` #:: txt ##:: rest => (SvText(txt, me = true), rest)
      case `SV_TEXT` #:: txt ##:: rest => (SvText(txt), rest)
      case `SV_FLAGMSG` #:: flag #:: 5 #:: actor #:: p #:: rest =>
        (Ignore(SV_FLAGMSG), rest)
      case `SV_FLAGMSG` #:: flag #:: message #:: actor #:: rest =>
        (Ignore(SV_FLAGMSG), rest)
      case `SV_SWITCHNAME` #:: name ##:: rest => (SwitchName(name), rest)
      case `SV_SWITCHTEAM` #:: t #:: rest => (SwitchTeam(t), rest)
      case `SV_SWITCHSKIN` #:: _ #:: _ #:: rest => (Ignore(SV_SWITCHSKIN), rest)
      case `SV_EDITMODE` #:: _ #:: rest => (Ignore(SV_EDITMODE), rest)
      case `SV_GAMEMODE` #:: _ #:: rest => (Ignore(SV_GAMEMODE), rest)
      case `SV_VOTERESULT` #:: _ #:: rest => (Ignore(SV_VOTERESULT), rest)
      case `SV_IPLIST` #:: rest =>
        val q = new CubeQueue(rest)
        val cnIps = Iterator.continually(q.getint).takeWhile(_ != -1).map(cn => cn -> q.getint).toVector
        (Ignore(SV_IPLIST), q.rest)
      case `SV_SETTEAM` #:: fpl #:: fnt #:: rest =>
        val fntM = fnt & 0x0f
        val ftr = fnt >> 4
        (SvSetteam(fpl, fntM), rest)
      case `SV_SPAWN` #:: rest =>

        val q = new CubeQueue(rest)
        val lifeSequence = q.getint
        val health = q.getint
        val armour = q.getint
        val gun = q.getint
        val ammos = (1 to 10).map(_ => q.getint).toVector
        val mags = (1 to 10).map(_ => q.getint).toVector
        (SvSpawn(lifeSequence, health, armour, gun, ammos, mags), q.rest)
      case `SV_THROWNADE` #:: rest =>
        val q = new CubeQueue(rest)
        (1 to 7).foreach(_ => q.getint)
        (Ignore(SV_THROWNADE), q.rest)
      case `SV_NEWMAP` #:: _ #:: rest => (Ignore(SV_NEWMAP), rest)
      case `SV_CLIENTPING` #:: _ #:: rest => (Ignore(SV_CLIENTPING), rest)
      case `SV_WEAPCHANGE` #:: num #:: rest => (WeaponChange(num), rest)
      case `SV_MAPIDENT` #:: _ #:: _ #:: rest => (Ignore(SV_MAPIDENT), rest)
      case `SV_VOTE` #:: _ #:: rest => (Ignore(SV_VOTE), rest)
      case `SV_CALLVOTE` #:: -1 #:: caller #:: nyes #:: nno #:: vtype #:: stuff =>
        val buf = new CubeQueue(stuff)
        voteSymbols(vtype) match {
          case 'SA_KICK | 'SA_BAN => buf.getint; buf.getstring
          case 'SA_MAP => buf.getstring; buf.getint; buf.getint
          case 'SA_SERVERDESC => buf.getstring
          case 'SA_STOPDEMO =>
          case 'SA_REMBANS =>
          case 'SA_SHUFFLETEAMS =>
          case 'SA_FORCETEAM => buf.getint; buf.getint
          case _ => buf.getint
        }
        (Ignore(SV_CALLVOTE), buf.rest)
      case `SV_CALLVOTE` #:: vtype #:: stuff =>
        val buf = new CubeQueue(stuff)
        voteSymbols(vtype) match {
          case 'SA_KICK | 'SA_BAN => buf.getint; buf.getstring
          case 'SA_MAP => buf.getstring; buf.getint; buf.getint
          case 'SA_SERVERDESC => buf.getstring
          case 'SA_STOPDEMO =>
          case 'SA_REMBANS =>
          case 'SA_SHUFFLETEAMS =>
          case 'SA_FORCETEAM => buf.getint; buf.getint
          case _ => buf.getint
        }
        (Ignore(SV_CALLVOTE), buf.rest)
      case other if other != ByteString.empty => assert(false, s"Should not have $other"); (Ignore(0), ByteString.empty)
    }
  }


}
