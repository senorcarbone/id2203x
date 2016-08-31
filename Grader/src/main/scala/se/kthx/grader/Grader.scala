package se.kthx.grader

import akka.actor._
import akka.pattern.ask
import akka.pattern.pipe
import spray.caching._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import spray.json._
import akka.routing.FromConfig
import com.larskroll.common.ActorDispatcher

object Grader {
    trait Request
    trait Response
    case object QueueStatus extends Request
    case object GradeStuff extends Request
    case object Shutdown extends Request
    case object ShutdownNow extends Request
    case class QueueLength(n: Long) extends Response
    case class QueueUnavailable(msg: String) extends Response
    case class ShutdownAck(msg: String) extends Response
}

class Grader extends Actor with ActorLogging {
    import Grader._
    import XQueueManager.{ GetQueueLength, PullSubmission }
    import XFileManager.Fetch
    import XQueueJsonProtocol._;

    implicit val ec = new ActorDispatcher(this)
    implicit val timeout = Timeout(60.seconds);

    val xqmanager = context.actorOf(Props[XQueueManager], "xqmanager");
    val xfilemanager = context.actorOf(Props[XFileManager], "xfilemanager");
    val checker = context.actorOf(FromConfig.props(SubmissionChecker.withManager(xfilemanager, self)), "checkerRouter");
    val statusCache: Cache[Either[Long, String]] = LruCache(1, 1, 5.seconds, 4.seconds);
    private var queueLenEstimate = -1l;
    private var outstandingChecks = 0l;
    private var shutdownInProgress = false;

    val scheduledCheck = context.system.scheduler.schedule(10.seconds, 60.seconds, self, QueueStatus);

    def receive = ActorDispatcher.receive orElse {
        case req: XQueueRequest => {
            if (req.xqueue_body != null) {
                sender ! Some(XQueueResponse(req.xqueue_header, XQueueResponseBody(false, 0, "That didn't work out...")))
            } else {
                sender ! None
            }
        }
        case QueueStatus => {
            val reply = sender
            log.debug("Got QueueStatus request");
            val ocf = statusCache("status")({
                queueLenEstimate = -1; // reset it
                (xqmanager ? GetQueueLength).mapTo[Either[Long, String]]
            });
            log.debug("Got future for the status");
            ocf map {
                case Left(n) => {
                    if (queueLenEstimate < 0) { // queue length unset
                        queueLenEstimate = n;
                    }
                    if (queueLenEstimate > 0) {
                        self ! GradeStuff
                    }
                    Left(QueueLength(n))
                }
                case Right(msg) => Right(QueueUnavailable(msg))
            } recover {
                case e =>
                    log.warning(s"Error when retrieving queue length:\n\t$e");
                    Right(QueueUnavailable(e.getMessage))
            } pipeTo reply
        }
        case GradeStuff => {
            if (shutdownInProgress) {
                log.info("Not executing scheduled pull as shutdown has been requested.");
            }
            while (queueLenEstimate > 0 && !shutdownInProgress) {
                outstandingChecks += 1;
                val subF = (xqmanager ? PullSubmission);
                val otherF = subF recover {
                    case e => SubmissionChecker.FailedPull(e.getMessage);
                } pipeTo checker;
                queueLenEstimate -= 1;
            }
        }
        case xqr: XQueueResponse => {
            xqmanager ! xqr
        }
        case XQueueManager.GradingCompleted => {
            outstandingChecks -= 1l;
            if (shutdownInProgress) {
                if (outstandingChecks == 0l) {
                    log.info("All outstanding checks completed. Shutting down.");
                    System.exit(0);
                } else {
                    log.info(s"There are $outstandingChecks checks remaining before shutdown.");
                }
            }
        }
        case Shutdown => {
            shutdownInProgress = true;
            if (outstandingChecks == 0l) {
                context.system.scheduler.scheduleOnce(5.seconds, self, ShutdownNow)
                val msg = "All outstanding checks completed. Shutting down in 5s.";
                log.info(msg);
                sender ! ShutdownAck(msg);
            } else {
                val msg = s"There are $outstandingChecks checks remaining before shutdown.";
                log.info(msg);
                sender ! ShutdownAck(msg);
            }
        }
        case ShutdownNow => {
            log.info(s"Shutting down now with $outstandingChecks outstanding checks remaining.");
            System.exit(0);
        }
    }
}

