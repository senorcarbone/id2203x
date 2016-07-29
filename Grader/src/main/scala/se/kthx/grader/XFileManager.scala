package se.kthx.grader

import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.util._
import spray.http.HttpCharsets._
import spray.httpx.encoding._
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.client.pipelining._
import spray.json._
import spray.httpx.SprayJsonSupport._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Success, Failure }
import spray.httpx.ResponseTransformation

object XFileManager {
    trait Request
    trait Response
    case class Fetch(fileUri: String) extends Request
}

class XFileManager extends Actor with ActorLogging {
    import XFileManager._;
    import context.dispatcher;
    val config = context.system.settings.config

    implicit val timeout = Timeout(60.seconds);

    val pipeline: SendReceive = sendReceive ~> decode(Deflate) ~> decode(Gzip);

    def receive = {
        case Fetch(fileUri) => {
            val reply = sender;
            val res = pipeline(Get(fileUri));
            res map {
                case response => if (response.status.isSuccess) {
                    Left(response.entity)
                } else {
                    Right(s"Error ${response.status} -- ${response.entity.asString}")
                }
            } recover {
                case error => Right(error.getMessage)
            } pipeTo reply
        }
    }
}