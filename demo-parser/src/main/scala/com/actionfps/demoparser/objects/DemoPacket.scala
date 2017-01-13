package com.actionfps.demoparser.objects

case class DemoPacket(millis: Int, chan: Int, data: ByteString)
