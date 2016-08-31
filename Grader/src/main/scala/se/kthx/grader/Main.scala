package se.kthx.grader

import akka.actor._
import akka.routing.FromConfig
import akka.io.IO
import spray.can.Http
import akka.event.Logging

object Main extends App {
    implicit val system = ActorSystem();

    val conf = system.settings.config;

    val nostopsupervisors = system.actorOf(Props[NoStopSupervisor], "nostop");
    val myListener = system.actorOf(FromConfig.props(Props[Router]), "routerRouter");
    
    val bindIf = conf.getString("kthx.grader.web-interface");
    val bindPort = conf.getInt("kthx.grader.web-port");


    IO(Http) ! Http.Bind(myListener, interface = bindIf, port = bindPort)
}