package se.kth.edx.id2203.templates

import se.sics.kompics.sl._
import se.sics.kompics.network._
import se.kth.edx.id2203.core.Ports._
import se.sics.kompics.{KompicsEvent, Start}
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}

import collection.mutable
import se.kth.edx.id2203.validation.checkBLE;


object GossipLeaderElection {
  case class CheckTimeout(timeout: ScheduleTimeout) extends Timeout(timeout);
  case class Heartbeat(ballot: Long) extends KompicsEvent;

  private val ballotOne = 0x0100000000l;

  private def ballotFromNAddress(n: Int, adr: Address): Long = {
    val nBytes = com.google.common.primitives.Ints.toByteArray(n);
    val addrBytes = com.google.common.primitives.Ints.toByteArray(adr.hashCode());
    val bytes = nBytes ++ addrBytes;
    val r = com.google.common.primitives.Longs.fromByteArray(bytes);
    assert(r > 0); // should not produce negative numbers!
    r
  }

  private def incrementBallotBy(ballot: Long, inc: Int): Long = {
    ballot + inc.toLong * ballotOne
  }

  private def incrementBallot(ballot: Long): Long = {
    ballot + ballotOne
  }
}

class GossipLeaderElection(init: Init[GossipLeaderElection]) extends ComponentDefinition {
  import GossipLeaderElection._

  val ble = provides(BallotLeaderElection);
  val pl = requires(PerfectLink);
  val timer = requires[Timer];

  val self = init match {
    case Init(s: Address) => s
  }
  val topology = cfg.getValue[List[Address]]("ble.simulation.topology");
  val delta = cfg.getValue[Long]("ble.simulation.delay");

  private var period = cfg.getValue[Long]("ble.simulation.delay");
  private val alive = {
    val elems = topology.map(a => (a -> None.asInstanceOf[Option[Long]]));
    mutable.Map(elems: _*)
  }
  private val suspected = mutable.Set.empty[Address];

  private var ballot = ballotFromNAddress(0, self);

  private var leader: Option[(Long, Address)] = None;

  private def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period);
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout));
    trigger(scheduledTimeout -> timer);
  }

  private def checkLeader() {
    val topProcess = alive.foldLeft((ballot, self)) {
      case (tmp, (p, Some(n))) if (n > tmp._1) => (n -> p)
      case (tmp, (_, Some(n))) if (n <= tmp._1) => tmp
      case (tmp, (_, None))  => tmp
    }

    leader match{
      case Some((bal, _)) =>
        if(topProcess != leader.get){
          if (topProcess._2 == self) {
            while (ballot < bal) {
              ballot = incrementBallotBy(ballot, 1);
            }
            leader = Some((ballot, self))
            trigger (BLE_Leader(self, ballot) -> ble);
          } else {
            leader = Some(topProcess)
            trigger (BLE_Leader(topProcess._2, topProcess._1) -> ble);
          }
        }
      case None =>
        leader = Some(topProcess)
        trigger (BLE_Leader(topProcess._2, topProcess._1) -> ble);
    }

  }

  ctrl uponEvent {
    case _: Start => handle {
      startTimer(period);
    }
  }

  timer uponEvent {
    case CheckTimeout(_) => handle {

      if (!alive.keySet.intersect(suspected).isEmpty) {
        period = period + delta;
      }

      for (p <- topology) {
        if (!alive.contains(p) && !suspected.contains(p)) {
          suspected += p;
        } else if (alive.contains(p) && suspected.contains(p)) {
          suspected -= p;
        }
        trigger(PL_Send(p, Heartbeat(ballot)) -> pl);
      }

      checkLeader();
      alive.clear();
      startTimer(period);
    }
  }

  pl uponEvent {
    case PL_Deliver(src, Heartbeat(n)) => handle {
      alive += (src -> Some(n))
    }
  }
}

object BLETemplate extends App {
  checkBLE[GossipLeaderElection];
}

