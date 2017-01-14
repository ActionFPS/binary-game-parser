package controllers

import java.nio.file.Files
import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source, StreamConverters}
import akka.util.ByteString
import com.actionfps.demoparser.objects.DemoPacket
import org.json4s.jackson.Serialization
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import play.api.mvc.{Action, Controller}

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
      val p = new java.lang.ProcessBuilder("ssh", "-i", privateKeyFilePath.toString, "demoread@woop.ac")
        .start()
      applicationLifecycle.addStopHook(() => Future.successful(p.destroy()))
      p.getInputStream
    })
      .scan(BufferedThingy.initial)(_.accept(_))
      .mapConcat(_.emit.toList)
      .map(l => l -> com.actionfps.demoparser.DemoAnalysis.extractBasicsz.lift.apply(l.data).toList)
      .mapConcat { case (b, l) => l.map(_._1) }
      .map(SomeResult)
      .runWith(Sink.asPublisher(true))

    ior
  }

  def getIt = Action {
    Ok.chunked(
      Source.fromPublisher(theSequence)
        .map { a =>

          import org.json4s._
          import org.json4s.jackson.Serialization.{read, write}
          implicit val fmt = DefaultFormats
          write(a)
        }
        .map(_ + "\n")
    )
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
