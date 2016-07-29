package se.kthx.grader

import akka.actor._
import akka.routing.FromConfig
import akka.io.IO
import spray.can.Http
import akka.event.Logging

object Main extends App {
    implicit val system = ActorSystem();

    val nostopsupervisors = system.actorOf(Props[NoStopSupervisor], "nostop");
    val myListener = system.actorOf(FromConfig.props(Props[Router]), "routerRouter");
    
    IO(Http) ! Http.Bind(myListener, interface = "localhost", port = 8080)
}