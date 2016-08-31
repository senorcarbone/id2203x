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
import spray.http.HttpHeaders._
import spray.http.MediaTypes._
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

    case object RetrySession
    class LoginException(m: String) extends RuntimeException(m)

}

class XQueueManager extends Actor with ActorLogging {
    import XQueueManager._;
    import XQueueJsonProtocol._;
    val config = context.system.settings.config

    implicit val timeout = Timeout(60.seconds);
    val maxRetryDepth = config.getInt("kthx.grader.max-retries");
    val xqueueUri = config.getString("kthx.grader.xqueue-uri");
    val xqueuePrefix = config.getString("kthx.grader.xqueue-prefix");
    val xqueueUser = config.getString("kthx.grader.xqueue-user");
    val xqueuePass = config.getString("kthx.grader.xqueue-password");
    val xqueueAuth = MultipartFormData(Seq(BodyPart(xqueueUser, "username"), BodyPart(xqueuePass, "password")));
    val xqueuePort = config.getInt("kthx.grader.xqueue-port");
    val xqueueName = config.getString("kthx.grader.xqueue-name");
    val xqueueData = Map(("queue_name" -> xqueueName));
    val xqueueLengthRequest = Get(Uri(xqueuePrefix + config.getString("kthx.grader.xqueue-api-length")).withQuery(xqueueData));
    val xqueuePullRequest = Get(Uri(xqueuePrefix + config.getString("kthx.grader.xqueue-api-pull")).withQuery(xqueueData));
    val xqueuePushPath = xqueuePrefix + config.getString("kthx.grader.xqueue-api-push");
    def xqueuePushResponse(entity: HttpEntity): HttpRequest = {
        Post(xqueuePushPath, entity)
    }
    val xqueueLoginPath = xqueuePrefix + config.getString("kthx.grader.xqueue-api-login");
    def xqueueLogin(): HttpRequest = {
        Post(xqueueLoginPath, xqueueAuth).withHeaders(List(`Accept`(`application/json`)))
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
    private var sessionMemo: Option[Session] = None;
    private var sessionOF: Option[Future[Session]] = None;
    private val outstandingRequests = scala.collection.mutable.Queue.empty[Tuple2[HttpRequest, Promise[HttpResponse]]];

    def session(): Option[Session] = {
        sessionMemo match {
            case Some(session) => Some(session)
            case None => {
                sessionOF match {
                    case Some(sessionF) => if (sessionF.isCompleted) {
                        log.debug(s"Session already established, but not memoed.")
                        Some(session.get)
                    } else {
                        log.debug(s"Session already being established.");
                        None
                    }
                    case None => {
                        val pipelineF: Future[SendReceive] =
                            for (
                                Http.HostConnectorInfo(connector, _) <- IO(Http)(context.system) ? Http.HostConnectorSetup(xqueueUri, port = xqueuePort, sslEncryption = true)
                            ) yield sendReceive(connector) ~> decode(Deflate) ~> decode(Gzip);

                        val sessionF = pipelineF flatMap { pipeline =>
                            val session = new Session(pipeline, xqueueAuth);
                            val respF = session(xqueueLogin());
                            respF map { resp =>
                                if (resp.status.isSuccess && resp.status == StatusCodes.OK) {
                                    val r = resp ~> forceContentType(`application/json`) ~> unmarshal[XQueueStringResponse];
                                    if (r.return_code != 0) {
                                        throw new LoginException(r.content)
                                    }
                                    session
                                } else {
                                    throw new LoginException(s"Unexpected response: $resp")
                                }
                            }
                        }
                        sessionF.onComplete {
                            case Success(session) =>
                                sessionMemo = Some(session);
                                log.info(s"Session to edX established.")
                            case Failure(error) =>
                                sessionMemo = None;
                                log.warning(s"Could not establish session with edX:\n$error");
                                retrySession()
                        }
                        None
                    }
                }
            }
        }
    }

    def retrySession() {
        context.system.scheduler.scheduleOnce(5.seconds, self, RetrySession);
    }

    def retryOutstanding() {
        session() match {
            case Some(session) => while (!outstandingRequests.isEmpty) {
                val (req, p) = outstandingRequests.dequeue
                val respF = session(req);
                p.completeWith(respF)
            }
            case None => log.warning("Can't retry outstanding requests. Session still not established.")
        }
    }

    def executeRequest(req: HttpRequest, delay: Boolean = true): Either[Future[HttpResponse], String] = {
        session() match {
            case Some(session) => Left(session(req))
            case None => {
                log.debug(s"XQueue connection not ready, yet. Delaying message $req")
                if (delay) {
                    val p = Promise[HttpResponse]();
                    outstandingRequests += (req -> p);
                    Left(p.future)
                } else {
                    Right("XQueue connection not ready, yet")
                }
            }
        }
    }

    def handleIfValid(response: HttpResponse): Option[HttpResponse] = {
        if (response.status.isSuccess && response.status == StatusCodes.OK) {
            Some(response)
        } else {
            log.info(s"Got a response with status code ${response.status}. Assuming session expired and resetting.")
            sessionMemo = None;
            session() // try to reconnect immediately
            None
        }
    }

    override def receive = ActorDispatcher.receive orElse {
        case RetrySession => {
            session() match {
                case Some(_) => log.info("Not retrying session as it seems to have been established by now.")
                case None    => log.info("Retrying session connection...")
            }
        }
        case GetQueueLength => {
            val reply = sender;
            executeRequest(xqueueLengthRequest, false) match {
                case Left(rf) => rf.map(handleIfValid) map {
                    case Some(response) => {
                        val r = response ~> forceContentType(`application/json`) ~> unmarshal[XQueueIntResponse];
                        Left(r.content.toLong)
                    }
                    case None => Right("Session currently inactive. Retry later.")
                } recover {
                    case e => Right(e.getMessage)
                } pipeTo reply
                case Right(msg) => reply ! Right(msg)
            }
        }
        case PullSubmission => {
            val reply = sender;
            executeRequest(xqueuePullRequest) match {
                case Left(rf) => rf.map(handleIfValid) map {
                    case Some(response) => {
                        val xqr = response ~> forceContentType(`application/json`) ~> unmarshal[XQueueStringResponse];
                        if (xqr.return_code == 0) {
                            SubmissionChecker.CheckSubmission(xqr.content.parseJson)
                        } else {
                            SubmissionChecker.FailedPull("Non-zero status code")
                        }
                    }
                    case None => SubmissionChecker.FailedPull("Invalid response message")
                } recover {
                    case e => SubmissionChecker.FailedPull(e.getMessage)
                } pipeTo reply
                case Right(msg) => reply ! Right(msg)
            }
        }
        case xqr: XQueueResponse => {
            val reply = sender;
            log.info(s"Graded submission with result: $xqr");
            val mpfd = MultipartFormData(Seq(
                BodyPart(xqr.xqueue_header.toJson.compactPrint, "xqueue_header"),
                BodyPart(xqr.xqueue_body.toJson.compactPrint, "xqueue_body")))
            val f: Future[GradingCompleted.type] = marshal(mpfd) match {
                case Right(entity) =>
                    def handleResponse(x: Either[Future[HttpResponse], String], depth: Int): Future[GradingCompleted.type] = {
                        x match {
                            case Left(rf) =>
                                rf.map(handleIfValid) flatMap {
                                    case Some(response) =>
                                        val xqsr = response ~> forceContentType(`application/json`) ~> unmarshal[XQueueStringResponse];
                                        if (xqsr.return_code == 0) {
                                            log.info(s"Pushing to XQueue was successful: $response"); Future.successful(GradingCompleted)
                                        } else {
                                            log.error(s"XQueue rejected pushed results: ${xqsr.content}");
                                            Future.successful(GradingCompleted) // not really, but this is probably not a temporary issue
                                        }
                                    case None =>
                                        if (depth < maxRetryDepth) {
                                            log.debug(s"Pushing to XQueue was unsucessful. Retrying ($depth)...")
                                            handleResponse(executeRequest(xqueuePushResponse(entity)), depth + 1)
                                        } else {
                                            log.warning(s"Could not push response to XQueue. Retry Limit exceeded."); Future.successful(GradingCompleted)
                                        }
                                } recover {
                                    case e =>
                                        log.warning(s"Could not push response to XQueue. Exception thrown in handler: $e");
                                        GradingCompleted

                                }
                            case Right(msg) => log.warning(s"Pushing to XQueue was unsuccessful. Error was: $msg"); Future.successful(GradingCompleted) // technically not, but this can't be fixed, so gotta give up here and decrement the counter anyway
                        }
                    }
                    handleResponse(executeRequest(xqueuePushResponse(entity)), 0)
                case Left(error) => log.error(s"Could not marshal response ($xqr): $error"); Future.successful(GradingCompleted) // technically not, but this can't be fixed, so gotta give up here and decrement the counter anyway
            }
            f pipeTo reply
        }
    }
}