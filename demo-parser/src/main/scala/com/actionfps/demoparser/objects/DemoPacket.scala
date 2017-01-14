package com.actionfps.demoparser.objects

case class DemoPacket(millis: Int, chan: Int, data: ByteString)

object DemoPacket {
  def unapply(input: ByteString): Option[(DemoPacket, ByteString)] = {
    extractDemoStuff(input)
  }
}

case class DemoPacketHeader(millis: Int, chan: Int, length: Int)

object DemoPacketHeader {
  def unapply(stuff: ByteString): Option[DemoPacketHeader] = {
    if (stuff.isEmpty) return None
    if (stuff.length < 12) return None
    val header = stuff.take(12)
    val (millis, chan, len) = {
      val buffer = header.asByteBuffer
      (java.lang.Integer.reverseBytes(buffer.getInt),
        java.lang.Integer.reverseBytes(buffer.getInt),
        java.lang.Integer.reverseBytes(buffer.getInt))
    }
    Some(DemoPacketHeader(millis, chan, len))
  }
}
