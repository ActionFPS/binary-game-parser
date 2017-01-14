package com.actionfps.demoparser.objects

case class DemoPacket(millis: Int, chan: Int, data: ByteString)

object DemoPacket {
  def unapply(input: ByteString): Option[(DemoPacket, ByteString)] = {
    extractDemoStuff(input)
  }
}
