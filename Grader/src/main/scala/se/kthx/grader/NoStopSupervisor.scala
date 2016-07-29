package se.kthx.grader

import akka.actor._
import scala.concurrent.duration._

class NoStopSupervisor extends Actor with ActorLogging {
    import akka.actor.SupervisorStrategy._

    val grader: ActorRef = context.actorOf(Props[Grader], "grader");
    
    override val supervisorStrategy =
        OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
            case e => log.warning(s"Restarting Grader after exception: $e"); Restart
        }

    def receive = {
        case x => log.warning(s"Got something, but I'm not handling anything: $x");
    }
}