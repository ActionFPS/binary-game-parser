package controllers

import java.nio.file.Files
import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source, StreamConverters}
import akka.util.ByteString
import com.actionfps.demoparser.objects.DemoPacket
import org.json4s.jackson.Serialization
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import play.api.mvc.{Action, Controller, WebSocket}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by me on 14/01/2017.
  */
@Singleton
class StreamIt @Inject()(configuration: Configuration, applicationLifecycle: ApplicationLifecycle)(implicit actorSystem: ActorSystem,
                                                                                                   executionContext: ExecutionContext) extends Controller {
  implicit val actorMat = ActorMaterializer()
  lazy val theSequence = {

    val privateKeyUrl = configuration.underlying.getString("private-key.url")
    //  IOUtils. new URL(privateKeyUrl).openStream()
    val privateKeyFilePath = Files.createTempFile("pk", "pk")

    import scala.sys.process._


    (new java.net.URL(privateKeyUrl) #> privateKeyFilePath.toFile).!


    val ior = StreamConverters.fromInputStream(() => {
      val p = new java.lang.ProcessBuilder("ssh",
        "-o", "UserKnownHostsFile=/dev/null",
        "-o", "StrictHostKeyChecking=no",
        "-i", privateKeyFilePath.toString,
        "demoread@woop.ac"
      )
        .start()
      applicationLifecycle.addStopHook(() => Future.successful(p.destroy()))
      p.getInputStream
    })
      .scan(BufferedThingy.initial)(_.accept(_))
      .mapConcat(_.emit.toList)
      .map(l => l -> com.actionfps.demoparser.DemoAnalysis.extractBasicsz.lift.apply(l.data).toList)
      .mapConcat { case (b, l) => l.map(_._1) }
      .map(SomeResult)
      .runWith(Sink.foreach(actorSystem.eventStream.publish))

    ior
  }

  def lines = {
    println(theSequence)
    Source.actorRef[SomeResult](100, OverflowStrategy.dropBuffer)
      .mapMaterializedValue{actorRef =>
        println(s"Subscribing me ... ${actorRef}")
        actorSystem.eventStream.subscribe(actorRef, classOf[SomeResult])
      }
      .mapConcat { a =>
        println(s"Got somethign - ${a}")
        import org.json4s._
        import org.json4s.jackson.Serialization.{read, write}
        implicit val fmt = DefaultFormats
        List(a, write(a))
      }
      .map(_ + "\n")
  }

  def getIt = Action {
    Ok.chunked(lines
    )
  }

  def getItWs() = {
    WebSocket.accept[String, String] { h =>
      Flow.apply[String].merge(lines)
    }
  }

}

case class BufferedThingy(byteString: ByteString, emit: Option[DemoPacket]) {
  def accept(bs: ByteString): BufferedThingy = {
    byteString ++ bs match {
      case DemoPacket(dp, leftover) => BufferedThingy(leftover, Some(dp))
      case other => BufferedThingy(other, None)
    }

  }
}

object BufferedThingy {
  val initial = BufferedThingy(ByteString.empty, None)
}

case class SomeResult(a: Any)
