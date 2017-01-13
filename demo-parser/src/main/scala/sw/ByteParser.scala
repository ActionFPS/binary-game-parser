package sw

import akka.util.ByteString

/**
  * Created by me on 07/08/2016.
  */
trait ByteParser[T] {
  def parse(input: Bytes): Option[(T, Bytes)]

  def unapply(input: Bytes): Option[(T, Bytes)] = parse(input)

  def old(byteString: ByteString): Option[(T, ByteString)] =
    parse(Bytes(byteString, 0)).map { case (t, b) => (t, b.zero.byteString) }
}

case class Bytes(byteString: ByteString, position: Int) {
  def zero = Bytes(byteString.drop(position), 0)
  def newBs = zero.byteString
}

object ByteParser {
  def apply[T](f: PartialFunction[Bytes, (T, Bytes)]) = new ByteParser[T] {
    override def parse(input: Bytes): Option[(T, Bytes)] = f.lift.apply(input)
  }
}
