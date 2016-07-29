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

object Grader {
    trait Request
    trait Response
    case object QueueStatus extends Request
    case object GradeStuff extends Request
    case class QueueLength(n: Long) extends Response
    case class QueueUnavailable(msg: String) extends Response
}

class Grader extends Actor with ActorLogging {
    import Grader._
    import XQueueManager.{ GetQueueLength, PullSubmission }
    import XFileManager.Fetch
    import XQueueJsonProtocol._;

    implicit val ec = context.dispatcher;
    implicit val timeout = Timeout(60.seconds);

    val xqmanager = context.actorOf(Props[XQueueManager], "xqmanager");
    val xfilemanager = context.actorOf(Props[XFileManager], "xfilemanager");
    val checker = context.actorOf(FromConfig.props(SubmissionChecker.withManager(xfilemanager, self)), "checkerRouter");
    val statusCache: Cache[Either[Long, String]] = LruCache(1, 1, 5.seconds, 4.seconds);
    private var queueLenEstimate = -1l;

    val scheduledCheck = context.system.scheduler.schedule(10.seconds, 60.seconds, self, QueueStatus);

    def receive = {
        case req: XQueueRequest => {
            if (req.xqueue_body != null) {
                sender ! Some(XQueueResponse(false, 0, "Didn't work bro!"))
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
            while (queueLenEstimate > 0) {
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
    }
}

