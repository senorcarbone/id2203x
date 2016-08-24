package se.kthx.grader

import akka.actor._
import spray.routing._
import spray.routing.authentication._
//import spray.routing.directives.CachingDirectives._
import spray.httpx.encoding._
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.http.MediaTypes._
import spray.http.StatusCodes._
import scala.concurrent._
import scala.util.{ Try, Success => Succ, Failure => Fail }
import scala.concurrent.duration._
import akka.util.Timeout
import akka.event.LoggingAdapter
import akka.pattern.{ ask, pipe }
import spray.routing.directives._
import spray.util._
import spray.json._
import spray.httpx.SprayJsonSupport._
import spray.caching._
import com.typesafe.config.Config

class Router extends Actor with ActorLogging with GradingService {
    def receive = runRoute(route)
    def actorRefFactory = context;
    //def selfRef = self;
    def ec = context.dispatcher;
    def logger = log;
    def config = context.system.settings.config
}

trait GradingService extends HttpService {
    import BasicDirectives._
    import ExecutionDirectives._
    import CachingDirectives._
    import XQueueJsonProtocol._

    def logger: LoggingAdapter;
    def config: Config;
    implicit def ec: ExecutionContext;
    def routeCache(maxCapacity: Int = 500, initialCapacity: Int = 16, timeToLive: Duration = Duration.Inf,
                   timeToIdle: Duration = Duration.Inf): Cache[RouteResponse] =
        LruCache(maxCapacity, initialCapacity, timeToLive, timeToIdle)
    //def selfRef: ActorRef;
    implicit val timeout = Timeout(10.seconds);

    val grader = actorRefFactory.actorSelection("/user/nostop/grader")

    def debugHandler(implicit log: LoggingContext) = ExceptionHandler {
        case e =>
            requestUri { uri =>
                log.warning("Request to {} could not be handled normally", uri);
                e.printStackTrace();
                complete(InternalServerError, "Nope, just no...")
            }
    }

    val respond = respondWithMediaType(`text/html`) & handleExceptions(debugHandler) & complete;

    def extractUser(userPass: UserPass): String = userPass.user

    val route: Route = {
        get {
            pathEndOrSingleSlash {
                (cache(routeCache(maxCapacity = 1, initialCapacity = 1)) & respond) {
                    HTMLLayout.withText("Grading Service is active.", "good").render
                }
            } ~ path("status") {
                respond {
                    showStatus()
                }
            } ~ path("shutdown") {
                authenticate(BasicAuth(realm = "KTHx Grader", createUser = extractUser _)) { userName =>
                    respond {
                        (grader ? Grader.Shutdown) map {
                            case Grader.ShutdownAck(msg) => HTMLLayout.withText(msg, "warn").render
                            case _                       => HTMLLayout.withText("Shutdown command not acknowledged!", "bad").render
                        } recover {
                            case _ => HTMLLayout.withText("Shutdown command not acknowledged!", "bad").render
                        }
                    }
                }
            } ~ cache(routeCache(maxCapacity = 16, initialCapacity = 4, timeToLive = 60.seconds)) {
                getFromResourceDirectory("WEB-INF")
            }
        }
    }

    def showStatus(): Future[String] = {
        import Grader._
        val f = (grader ? QueueStatus).mapTo[Either[QueueLength, QueueUnavailable]]
        f.map {
            case Left(ql) =>
                HTMLLayout.withText(s"Current grading queue length is ${ql.n}", "good").render
            case Right(qu) =>
                HTMLLayout.withText(s"Could not get grading queue length. Reason: ${qu.msg}", "bad").render
        }.recover {
            case e =>
                logger.warning(s"Couldn't get xqueue length. Error was:\n	$e");
                HTMLLayout.withText("Queue status unkown.", "bad").render
        }
    }
}

