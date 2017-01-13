package af.live

import java.io.{DataInputStream, FileInputStream, InputStream}
import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteString
import com.actionfps.demoparser.DemoAnalysis

object ListenerApp extends App {
  val fis = new FileInputStream("samples.pcap")
  try {
    val dis = new DataInputStream(fis)
    try {
      val i = dis.readInt()
      assert(i == 0xd4c3b2a1)
      dis.skipBytes(2 + 2 + 4 + 4 + 4 + 4)
      val buf: Array[Byte] = Array.fill(1024)(Byte.MinValue)
var cnt = 0
      while(dis.available() != 0 && cnt < 10) {
        cnt = cnt + 1
        val ts = K.readInt(dis)
        val usec = dis.readInt()
        val len = K.readInt(dis)
        dis.readInt()
        dis.readFully(buf, 0, len)
        val bf2 = ByteString.fromArray(buf, 0, len)
        val bb = bf2.asByteBuffer.order(ByteOrder.BIG_ENDIAN)
        val lenUdpData = bb.getShort(38) - 8
//        println("Len = " + lenUdpData)
        println("A = " + bb.getShort(42))
//        println(bb.getShort() + " => " +java.lang.Short.reverseBytes(bb.getShort())) // peerId
//        println(len)
//        val shift = 18
//        val bar = ByteString.fromArray(buf, 42 + shift, len - 42 - shift)
//        println(bar)
//        val r = DemoAnalysis.extractBasicsz(bar)
//        println(r)
      }

    } finally dis.close()
  } finally fis.close()
}

object K {
  def readInt(inputStream: InputStream): Int = {
    val a = inputStream.read()
    val b = inputStream.read()
    val c = inputStream.read()
    val d = inputStream.read()
    readInt(a, b, c, d)

  }

  def readInt(a: Int, b: Int, c: Int, d: Int): Int = {
    a + (b << 8) + (c << 16) + (d << 24)
  }
}
