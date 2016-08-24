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
import com.larskroll.common.ActorDispatcher

object XQueueManager {
    trait Request
    trait Response
    case object GetQueueLength extends Request
    case object PullSubmission extends Request
    case class QueueLength(n: Int) extends Response
    case object GradingCompleted extends Response
}

class XQueueManager extends Actor with ActorLogging {
    import XQueueManager._;
    import XQueueJsonProtocol._;
    val config = context.system.settings.config

    implicit val timeout = Timeout(60.seconds);
    val xqueueUri = config.getString("kthx.grader.xqueue-uri");
    val xqueuePort = config.getInt("kthx.grader.xqueue-port");
    val xqueueName = config.getString("kthx.grader.xqueue-name");
    val xqueueLengthRequest = Get("/" + xqueueName + "/" + config.getString("kthx.grader.xqueue-api-length"));
    val xqueuePullRequest = Get("/" + xqueueName + "/" + config.getString("kthx.grader.xqueue-api-pull"));
    val xqueuePushPath = "/" + xqueueName + "/" + config.getString("kthx.grader.xqueue-api-push")
    def xqueuePushResponse(entity: HttpEntity): HttpRequest = {
        Post(xqueuePushPath, entity)
    }

    implicit val LongUnmarshaller = new Unmarshaller[Long] {
        def apply(entity: HttpEntity) = try {
            val bodyS = entity.asString;
            val n = bodyS.toLong
            Right(n)
        } catch {
            case e: Throwable => Left(MalformedContent(s"Couldn't deserialise $entity", e))
        }
    }

    implicit val ec = new ActorDispatcher(this)

    val pipelineF: Future[SendReceive] =
        for (
            Http.HostConnectorInfo(connector, _) <- IO(Http)(context.system) ? Http.HostConnectorSetup(xqueueUri, port = xqueuePort)
        ) yield sendReceive(connector) ~> decode(Deflate) ~> decode(Gzip);

    private var pipelineMemo: Option[SendReceive] = None;

    def executeRequest(req: HttpRequest): Either[Future[HttpResponse], String] = {
        pipelineMemo match {
            case Some(pipeline) => Left(pipeline(req))
            case None => {
                pipelineF.value match {
                    case Some(Success(pipeline)) =>
                        pipelineMemo = Some(pipeline);
                        Left(pipeline(req))
                    case Some(Failure(e)) =>
                        context.self ! Kill;
                        Right("Couldn't connect to XQueue")
                    case None => Right("XQueue manager not ready, yet.")
                }
            }
        }
    }

    override def receive = ActorDispatcher.receive orElse {
        case GetQueueLength => {
            val reply = sender;
            executeRequest(xqueueLengthRequest) match {
                case Left(rf) => rf map {
                    case response =>
                        response.entity.as[Long] match {
                            case Right(len)  => Left(len);
                            case Left(error) => Right(error.toString())
                        }
                } recover {
                    case e => Right(e.getMessage)
                } pipeTo reply
                case Right(msg) => reply ! Right(msg)
            }
        }
        case PullSubmission => {
            val reply = sender;
            executeRequest(xqueuePullRequest) match {
                case Left(rf) => rf map {
                    case response => SubmissionChecker.CheckSubmission(response.entity)
                } recover {
                    case e => SubmissionChecker.FailedPull(e.getMessage)
                } pipeTo reply
                case Right(msg) => reply ! Right(msg)
            }
        }
        case xqr: XQueueResponse => {
            val reply = sender;
            log.info(s"Graded submission with result: $xqr");
            val fo = marshal(xqr) match {
                case Right(entity) => executeRequest(xqueuePushResponse(entity)) match {
                    case Left(rf) =>
                        val ret = rf map {
                            case response => if (response.status.isSuccess) {
                                log.info(s"Pushing to XQueue was successful: $response")
                            } else {
                                log.warning(s"Pushing to XQueue was unsuccessful. Error was: $response")
                            }
                        } recover {
                            case e => log.warning(s"Pushing to XQueue was unsuccessful. Error was: $e")
                        };
                        Some(ret)
                    case Right(msg) => log.warning(s"Pushing to XQueue was unsuccessful. Error was: $msg"); None
                }
                case Left(error) => log.warning(s"Could not marshal response ($xqr): $error"); None
            }
            fo match {
                case Some(f) => f map {_ => GradingCompleted} pipeTo reply
                case None => reply ! GradingCompleted
            }
        }
    }
}