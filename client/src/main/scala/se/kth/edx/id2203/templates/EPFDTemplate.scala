//HOMEWORK 1 - Implement EPFD, defined bellow

package se.kth.edx.id2203.templates

import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.validation.validation._
import se.sics.kompics.network._
import se.sics.kompics.sl._
import se.sics.kompics.sl.Init
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{ComponentDefinition => _, Port => _, Start, KompicsEvent}
import scala.collection.mutable._


//Define initialization event

//Define EPFD Implementation
class EPFD(epfdInit: Init[EPFD]) extends ComponentDefinition {

  //EPFD subscriptions
  val timer = requires[Timer];
  val pp2p = requires[PerfectP2PLink];
  val epfd = provides[EventuallyPerfectFailureDetector];

  // EPDF component state and initialization
  val self = epfdInit match {case Init(s: Address) => s}
  val topology = cfg.getValue[List[Address]]("epfd.simulation.topology");
  val delta = cfg.getValue[Long]("epfd.simulation.delay");

  var period = cfg.getValue[Long]("epfd.simulation.delay");
  var alive = Set(cfg.getValue[List[Address]]("epfd.simulation.topology"): _*);
  var suspected = Set[Address]();

  //Declare custom message types related to internal component implementation
  case class CheckTimeout(timeout: ScheduleTimeout) extends Timeout(timeout);

  case class Heartbeat() extends KompicsEvent;


  def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period);
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout));
    trigger(scheduledTimeout -> timer);
  }

  //EPFD event handlers
  ctrl uponEvent {
    case _: Start => handle {
      println("EPFD Started");
        startTimer(period);
    }
  }

  timer uponEvent {
    case CheckTimeout(_) => handle {
      if (!alive.intersect(suspected).isEmpty) {
        period = period + delta;
      }

      for (p <- topology) {
        if (!alive.contains(p) && !suspected.contains(p)) {
          suspected = suspected + p;
          trigger(Suspect(p) -> epfd);
        } else if (alive.contains(p) && suspected.contains(p)) {
          suspected = suspected - p;
          trigger(Restore(p) -> epfd);
        }
        trigger(Pp2pSend(p, Heartbeat()) -> pp2p);
      }

      alive = Set[Address]();
      startTimer(period);
    }
  }

  pp2p uponEvent {
    case Pp2pDeliver(src, Heartbeat()) => handle {
      //      println(s"Heartbeat received from  $src. to $self . Adding it to the list");
      alive = alive + src;
    }
  }

};


object EPFDTemplate extends App  {
  checkEPFD[EPFD]();    
}
