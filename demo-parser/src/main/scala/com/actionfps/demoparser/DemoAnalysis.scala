package com.actionfps.demoparser

import akka.util.ByteString
import com.actionfps.demoparser.objects._
import com.actionfps.demoparser.objects.SvClientSubMessage.{SvSetteam, SvSpawn, SwitchName, SwitchTeam}
import org.json4s.NoTypeHints

import scala.annotation.tailrec

object DemoAnalysis {

  implicit val formats = org.json4s.jackson.Serialization.formats(NoTypeHints)

  case class TeamState(players: Map[Int, GameClient], flagAway: Option[PositionVector], flagCarriedBy: Option[Int], flags: Int)

  case class GameClient(name: String, position: PositionVector, yaw: Float, alive: Boolean, shooting: Boolean)

  case class GameState(map: String, millis: Int = 0, teams: Map[Int, TeamState]) {
    val mapDefs = MapDefs.get(map)

    def defaultFlagPosition(team: Int) = {
      for {MapDefCtfFlag(x, y, _) <- mapDefs.flags.find(_.tn == team)}
        yield mapDefs.dims.pixelToPosition(x, y)
    }

    def defaultFlagPixels(team: Int) = {
      for {
        mdf@MapDefCtfFlag(x, y, _) <- mapDefs.flags.find(_.tn == team)
      } yield mapDefs.dims.getPixelPosition(x, y)
    }

    def toJson = {
      val players = for {
        (tid, team) <- teams
        (cn, player) <- team.players
        (px, py) = mapDefs.dims.getPixelPosition(player.position)
      } yield Map("name" -> player.name, "x" -> px.toInt, "y" -> py.toInt, "team" -> tid, "alive" -> player.alive, "yaw" -> player.yaw, "shooting" -> player.shooting)
      val flags = for {
        (tid, team) <- teams
        (px, py) <- team.flagAway.map(mapDefs.dims.getPixelPosition).orElse(defaultFlagPixels(tid))
      } yield Map("team" -> tid, "x" -> px.toInt, "y" -> py.toInt, "flags" -> team.flags)
      val stuff = Map("millis" -> millis, "map" -> map, "flags" -> flags, "players" -> players)
      stuff
    }
  }

  @tailrec
  def parsePacket(packet: ByteString, accum: Stream[Any] = Stream.empty): Stream[Any] = {
    extractBasicsz.lift.apply(packet) match {
      case Some((stuff, rest)) if rest.nonEmpty => parsePacket(rest, accum :+ stuff)
      case Some((stuff, _)) => accum :+ stuff
      case None => accum
    }
  }

  val extractBasicsz =
    Function.unlift(PositionResults.parse) orElse
      Function.unlift(SvClients.parse) orElse
      Function.unlift(Died.parse) orElse
      Function.unlift(FlagUpdates.parse) orElse
      Function.unlift(FlagCount.parse) orElse
      Function.unlift(ClientDisconnected.parse)

  case class MapDefCtfFlag(x: Int, y: Int, tn: Int)

  object MapDefCtfFlag {
    val r = """ctf\-flag=\(x=(\d+),y=(\d+),a1=\d+,a2=(\d+),a3=\d+,a4=\d+\)""".r

    def fromString(input: String) = Option(input).collectFirst {
      case r(x, y, t) => MapDefCtfFlag(x.toInt, y.toInt, t.toInt)
    }
  }

  case class MapDims(ax: Int, ay: Int, bx: Int, by: Int) {
    val xspan = bx - ax
    val yspan = by - ay
    val iw = 1024
    val ih = 1024
    val mul = if (xspan >= yspan) (iw / xspan.toFloat) else (ih / yspan.toFloat)

    def getPixelPosition(x: Float, y: Float): (Float, Float) = {
      ((x - ax) * mul, (y - ay) * mul)
    }

    def getPixelPosition(positionVector: PositionVector): (Float, Float) =
      getPixelPosition(positionVector.x, positionVector.y)

    def invert(x: Float, y: Float): (Float, Float) = {
      ((x / mul) + ax, ((y / mul) + ay))
    }

    def pixelToPosition(x: Float, y: Float): PositionVector = {
      val (a, b) = invert(x, y)
      PositionVector(a, b, 0)
    }
  }

  object MapDims {
    val r = """Map dims: \((\d+), (\d+)\) -> \((\d+), (\d+)\)""".r

    def fromString(input: String) = Option(input).collectFirst {
      case r(ax, ay, bx, by) => MapDims(ax.toInt, ay.toInt, bx.toInt, by.toInt)
    }
  }

  case class MapDefs(name: String, dims: MapDims, flags: Vector[MapDefCtfFlag])

  object MapDefs {
    val cache = scala.collection.mutable.Map.empty[String, MapDefs]

    def get(map: String) = {
      cache.getOrElseUpdate(map, load(map))
    }

    def load(map: String) = {
      val filename = scala.util.Properties.userDir + java.io.File.separator + "ui" + java.io.File.separator + "mapshots" + java.io.File.separator + map + ".txt"
      val lines = scala.io.Source.fromFile(filename).getLines().toVector
      val flags = lines.flatMap(MapDefCtfFlag.fromString)
      val dims = lines.flatMap(MapDims.fromString).head
      MapDefs(map, dims, flags)
    }
  }

  implicit def welcomeToGameState(welcome: Welcome): GameState = {
    val teams = for {
      team <- Vector(0, 1)
    } yield team -> TeamState(
      flagAway = None,
      flags = 0,
      flagCarriedBy = None,
      players = {
        for {
          client <- welcome.resume.clients
          if client.team == team
          player <- welcome.resume.players
          if client.cn == player.cn
          cn = client.cn
          gc = GameClient(
            name = client.name,
            position = PositionVector.empty,
            yaw = 0.0f,
            alive = false,
            shooting = false
          )
        } yield cn -> gc
      }.toMap
    )
    GameState(
      map = welcome.mapChange.name,
      teams = teams.toMap,
      millis = 0
    )
    //    val uts = for { (tid, t) <- gs1.teams
    //          fp <- gs1.mapDefs.flags.find(_.tn == tid)} yield tid -> t.copy(flagPosition = gs1.mapDefs.dims.pixelToPosition(fp.x, fp.y))
    //    gs1.copy(teams = gs1.teams ++ uts)
  }


  val go = objects.demoPacketsStream _

  def goThroughPackets(input: ByteString) = {
    go(input).flatMap(bs => Welcome.parse(bs.data)).head
    go(input).flatMap(e => parsePacket(e.data).map(r => e.millis -> r))
  }
  def goThroughPackets2(input: ByteString) = {
    val h = go(input).flatMap(bs => Welcome.parse(bs.data)).head
    val r = go(input).flatMap(e => parsePacket(e.data).map(r => e.millis -> r))
    (h, r)
  }

  def flowState(input: ByteString) = {
    val initialState = go(input).flatMap(bs => Welcome.parse(bs.data)).head._1: GameState
    go(input).flatMap(e => parsePacket(e.data).map(r => e.millis -> r)).scanLeft(initialState) {
      case (state, (millis, Died(actor, victim, _, _, _))) =>
        val updatedTeams = for {
          (tid, t) <- state.teams
          p <- t.players.get(victim)
        } yield tid -> t.copy(players = t.players.updated(victim, p.copy(alive = false, shooting = false)))
        state.copy(millis = millis, teams = state.teams ++ updatedTeams)
      case (s, (millis, FlagCount(cn, _))) =>
        // player scored flag
        val updatedTeams = for {
          (tid, t) <- s.teams
          if t.players.contains(cn)
        } yield tid -> t.copy(flags = t.flags + 1)
        s.copy(millis = millis, teams = s.teams ++ updatedTeams)
      case (s, (millis, FlagUpdates(fis))) =>
        fis.foldLeft(s) {
          case (state, fu@FlagUpdate(tid, flagState, carrying, dropped)) =>
            val teamUpdateO = Option(fu).flatMap {
              case _ if fu.inBase =>
                for {
                  t <- state.teams.get(tid)
                } yield tid -> t.copy(flagAway = None, flagCarriedBy = None)
              case _ if fu.stolen =>
                for {
                  t <- state.teams.get(tid)
                  cn <- carrying
                  carryPosition <- (for {
                    (ttid, tt) <- state.teams
                    (`cn`, p) <- tt.players
                  } yield p.position).headOption
                } yield tid -> t.copy(flagAway = Option(carryPosition), flagCarriedBy = Option(cn))
              case _ if fu.wasDropped =>
                for {
                  t <- state.teams.get(tid)
                  d <- dropped
                } yield tid -> t.copy(flagAway = Option(d), flagCarriedBy = None)
              case _ if fu.idle => None
            }
            state.copy(millis = millis, teams = state.teams ++ teamUpdateO.toVector)
        }
      case (state, (millis, PositionResults(pres))) =>
        pres.foldLeft(state) {
          case (s, PositionResult(cn, _, pos, _, yaw, _, _, _, _, shooting)) =>
            val updatedTeam = for {
              (teamnum, team) <- s.teams
              (`cn`, player) <- team.players
            } yield teamnum -> team.copy(players = team.players.updated(cn, player.copy(position = pos, yaw = yaw, shooting = shooting)))
            val A = s.copy(teams = s.teams ++ updatedTeam)
            val updatedFlagPosition = for {
              (teamnum, team) <- A.teams
              cb <- team.flagCarriedBy
              if cb == cn
            } yield teamnum -> team.copy(flagAway = Option(pos))
            A.copy(teams = A.teams ++ updatedFlagPosition)
        }.copy(millis = millis)
      case (state, (millis, ClientDisconnected(cn))) =>
        val updatedTeam = for {
          (teamnum, team) <- state.teams
        } yield teamnum -> team.copy(players = team.players - cn)
        state.copy(teams = state.teams ++ updatedTeam)
      case (state, (millis, SvClients(stuffs))) =>
        val spawnedPlayers = for {
          SvClient(cn, svs) <- stuffs
          it <- svs
          if it.isInstanceOf[SvSpawn]
        } yield cn
        val switchedNames = for {
          SvClient(cn, svg) <- stuffs
          SwitchName(newName) <- svg
        } yield cn -> newName
        val switchedTeams = (for {
          SvClient(cn, svg) <- stuffs
          SwitchTeam(team) <- svg
        } yield cn -> team) ++ (
          for {
            SvClient(_, svg) <- stuffs
            SvSetteam(cn, toteam) <- svg
          } yield cn -> toteam
          )
        val withSwitchedTeamsState = switchedTeams.foldLeft(state) {
          case (s, (cn, targetTeamNum)) =>
            val updatedTeams = for {
              targetTeam <- s.teams.get(targetTeamNum).toVector
              otherTeamNum = 1 - targetTeamNum
              otherTeam <- s.teams.get(otherTeamNum).toVector
              thePlayer <- otherTeam.players.get(cn).toVector
              newTargetTeam = targetTeam.copy(players = targetTeam.players.updated(cn, thePlayer))
              newSourceTeam = otherTeam.copy(players = otherTeam.players - cn)
              (tid, t) <- Vector(targetTeamNum -> newTargetTeam, otherTeamNum -> newSourceTeam)
            } yield tid -> t
            s.copy(teams = s.teams ++ updatedTeams)
        }
        val withSwitchedNamesState = switchedNames.foldLeft(withSwitchedTeamsState) {
          case (s, (cn, newName)) =>
            val updatedTeams = for {(tid, t) <- s.teams; p <- t.players.get(cn)}
              yield tid -> t.copy(players = t.players.updated(cn, p.copy(name = newName)))
            s.copy(teams = s.teams ++ updatedTeams)
        }
        val withSpawnedPlayersState = spawnedPlayers.foldLeft(withSwitchedNamesState) {
          case (s, cn) =>
            val updatedTeams = for {(tid, t) <- s.teams; p <- t.players.get(cn)}
              yield tid -> t.copy(players = t.players.updated(cn, p.copy(alive = true)))
            s.copy(teams = s.teams ++ updatedTeams)
        }
        withSpawnedPlayersState.copy(millis = millis)
    }
  }

}
