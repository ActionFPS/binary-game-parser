package com.actionfps.demoparser.objects

/**
* Created by me on 06/08/2016.
*/
object PositionResults {
  def parse(input: ByteString) = {
    Option(input) collectFirst {
      case (`SV_POSC` | `SV_POS`) #:: rest =>
        def go(bs: ByteString, accum: Vector[PositionResult]): (Vector[PositionResult], ByteString) = {
          PositionResult.parse(bs) match {
            case Some((stuff, other)) => go(other, accum :+ stuff)
            case None => (accum, bs)
          }
        }
        val (items, leftOver) = go(input, Vector.empty)
        assert(leftOver.isEmpty, s"Position parsing incomplete, have $leftOver from $input, with items $items")
        (PositionResults(items), ByteString.empty)
    }
  }
}

case class PositionResults(list: Vector[PositionResult])
