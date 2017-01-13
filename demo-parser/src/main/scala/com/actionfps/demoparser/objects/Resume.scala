package com.actionfps.demoparser.objects

import sw.{ByteParser, Bytes}

/**
  * Created by me on 06/08/2016.
  */
object Resume {

  def parse(byteString: ByteString, cln: Int) = byteParser(cln).parse(Bytes(byteString, 0)).map { case (r, bs) => (r, bs.zero.byteString) }

  def byteParser(cln: Int) = ByteParser[Resume] {
    case `SV_RESUME` #:: stuff =>
      def readPlayers(in: ByteString, accum: Vector[PlayerInfo]): (Vector[PlayerInfo], ByteString) = {
        if (accum.size == cln) (accum, in)
        else {
          PlayerInfo.parse(in) match {
            case Some((player, re)) =>
              readPlayers(re, accum :+ player)
            case None =>
              (accum, in)
          }
        }
      }
      val (players, -1 #:: rest) = readPlayers(stuff.zero.byteString, Vector.empty)
      def readClients(in: ByteString, accum: Vector[InitClient]): (Vector[InitClient], ByteString) = {
        InitClient.parse(in) match {
          case Some((client, re)) => readClients(re, accum :+ client)
          case None => (accum, in)
        }
      }
      val (clients, afterResume) = readClients(rest, Vector.empty)
      (Resume(players, clients), Bytes(afterResume, 0))
  }
}

case class Resume(players: Vector[PlayerInfo], clients: Vector[InitClient])
