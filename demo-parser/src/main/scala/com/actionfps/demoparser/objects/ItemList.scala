package com.actionfps.demoparser.objects

import com.actionfps.demoparser.Compressor
import sw.{ByteParser, Bytes}

/**
  * Created by me on 06/08/2016.
  */
object ItemList {

  val byteParser = ByteParser[ItemList] {
    case `SV_ITEMLIST` #:: rest =>
      def items(cb: ByteString, accum: Vector[Int]): (Vector[Int], ByteString) = {
        Compressor.shiftInt(cb) match {
          case Some((-1, left)) =>
            (accum, left)
          case Some((n, oth)) =>
            items(oth, accum :+ n)
          case None =>
            (accum, cb)
        }
      }
      val (itemNums, leftOvers) = items(rest.zero.byteString, Vector.empty)
      def getFlags(cb: ByteString, accum: Vector[FlagUpdate]): (Vector[FlagUpdate], ByteString) = {
        FlagUpdate.parse(cb) match {
          case Some((flag, taila)) => getFlags(taila, accum :+ flag)
          case None => (accum, cb)
        }
      }
      val (flagsInfos, tail) = getFlags(leftOvers, Vector.empty)
      (ItemList(itemNums, flagsInfos), Bytes(tail, 0))
  }
}

case class ItemList(items: Vector[Int], flags: Vector[FlagUpdate])
