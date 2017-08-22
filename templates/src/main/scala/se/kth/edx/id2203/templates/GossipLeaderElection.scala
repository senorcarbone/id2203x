package se.kth.edx.id2203.templates

import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.validation.checkBLE
import se.sics.kompics.network._
import se.sics.kompics.sl._
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{KompicsEvent, Start}

import scala.collection.mutable;

object GossipLeaderElection {

  case class CheckTimeout(timeout: ScheduleTimeout) extends Timeout(timeout);

  case class HeartbeatReq(round: Long, highestBallot: Long) extends KompicsEvent;

  case class HeartbeatResp(round: Long, ballot: Long) extends KompicsEvent;

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
  private val ballots = mutable.Map.empty[Address, Long];

  private var round = 0l;
  private var ballot = ballotFromNAddress(0, self);

  private var leader: Option[(Long, Address)] = None;
  private var highestBallot: Long = ballot;

  private def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period);
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout));
    trigger(scheduledTimeout -> timer);
  }

  private def becomeLeader() {
    while (ballot <= highestBallot) {
      ballot = incrementBallotBy(ballot, 1);
    }
    //    highestBallot = ballot;
    leader = Some((ballot, self))
    trigger(BLE_Leader(self, ballot) -> ble);
  }

  private def makeLeader(topProcess: (Long, Address)) {
    highestBallot = topProcess._1;
    leader = Some(topProcess)
    trigger(BLE_Leader(topProcess._2, topProcess._1) -> ble);
  }

  private def checkLeader() {
    val topProcess = ballots.foldLeft((ballot, self)) {
      case (acc, (p, n)) if (n > acc._1) => (n -> p)
      case (acc, (_, n)) if (n <= acc._1) => acc
    }

    if (topProcess._1 < highestBallot) {
      becomeLeader(); // either the old leader has died or we didn't have one, yet, or the timeouts aren't settled, yet
    } else {
      leader match {
        case Some((b, p)) => {
          if (topProcess != leader.get) {
            if (topProcess._2 == self) {
              becomeLeader();
            } else {
              makeLeader(topProcess);
            }
          } // else no change
        }
        case None => {
          makeLeader(topProcess);
        }
      }
    }
  }

  ctrl uponEvent {
    case _: Start => handle {
      startTimer(period);
    }
  }

  timer uponEvent {
    case CheckTimeout(_) => handle {
      checkLeader();

      round += 1;
      for (p <- topology) {
        if (p != self)
          trigger(PL_Send(p, HeartbeatReq(round, highestBallot)) -> pl);
      }

      ballots.clear();
      startTimer(period);
    }
  }

  pl uponEvent {
    case PL_Deliver(src, HeartbeatReq(r, hb)) => handle {
      //      if (hb > highestBallot) {
      //        highestBallot = hb;
      //      }
      trigger(PL_Send(src, HeartbeatResp(r, ballot)) -> pl);
    }
    case PL_Deliver(src, HeartbeatResp(r, b)) => handle {
      if (r == round) {
        ballots += (src -> b)
      } else {
        period += delta;
        //        if (b > highestBallot) {
        //          highestBallot = b;
        //        }
      }
    }
  }
}

object BLETemplate extends App {
  checkBLE[GossipLeaderElection];
}

