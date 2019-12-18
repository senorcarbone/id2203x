package se.kth.edx.id2203.validation.testers.TesterPrimitives

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils
import se.kth.edx.id2203.core.Ports._
import se.sics.kompics.{KompicsEvent, Start}
import se.sics.kompics.network.Address
import se.sics.kompics.sl.{ComponentDefinition, _}
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}

import scala.collection.immutable.Set
import scala.collection.mutable

object TesterPrimitives {

  case class LECheckTimeout(timeout: ScheduleTimeout) extends Timeout(timeout);

  case class LEHeartbeatReq(round: Long, highestBallot: Long) extends KompicsEvent;

  case class LEHeartbeatResp(round: Long, ballot: Long) extends KompicsEvent;

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
  
  
  class ProvidedBroadcast(init: Init[ProvidedBroadcast]) extends ComponentDefinition {
    val pp2p = requires[PerfectLink];
    val beb = provides[BestEffortBroadcast];
    val (self, topology) = init match {
      case Init(s: Address, t: Set[Address] @unchecked) => (s, t)
    };
    beb uponEvent {
      case x: BEB_Broadcast => {
        topology.foreach(addr => trigger(PL_Send(addr, x) -> pp2p));
      }
    }
    pp2p uponEvent {
      case PL_Deliver(src, BEB_Broadcast(payload)) => {
        trigger(BEB_Deliver(src, payload) -> beb);
      }
    }
   }                
  

  class ProvidedLeaderElection(init: Init[ProvidedLeaderElection]) extends ComponentDefinition {

    val ble = provides[BallotLeaderElection];
    val pl = requires[PerfectLink];
    val timer = requires[Timer];

    val self = init match {
      case Init(s: Address) => s
    }
    val topology = cfg.getValue[Set[Address]]("ble.simulation.topology");
    val delta = cfg.getValue[Long]("ble.simulation.delay");
    val majority = (topology.size / 2) + 1;

    private var period = cfg.getValue[Long]("ble.simulation.delay");
    private val ballots = mutable.Map.empty[Address, Long];

    private var round = 0l;
    private var ballot = ballotFromNAddress(0, self);

    private var leader: Option[(Long, Address)] = None;
    private var highestBallot: Long = ballot;

    private def startTimer(delay: Long): Unit = {
      val scheduledTimeout = new ScheduleTimeout(period);
      scheduledTimeout.setTimeoutEvent(LECheckTimeout(scheduledTimeout));
      trigger(scheduledTimeout -> timer);
    }

    private def makeLeader(topProcess: (Long, Address)) {
      highestBallot = topProcess._1;
      leader = Some(topProcess)
      trigger(BLE_Leader(topProcess._2, topProcess._1) -> ble);
    }

    private def checkLeader() {
      if ((ballots.size + 1) < majority) {
        return ; // we can't talk to enough people to even try to elect a leader...just wait
      }
      val topProcess = ballots.foldLeft((ballot, self)) {
        case (acc, (p, n)) if (n > acc._1)  => (n -> p)
        case (acc, (_, n)) if (n <= acc._1) => acc
      }

      if (topProcess._1 < highestBallot) {
        while (ballot <= highestBallot) {
          ballot = incrementBallotBy(ballot, 1);
        }
        leader = None;
      } else {
        leader match {
          case Some(l) => {
            if (topProcess != l) {
              makeLeader(topProcess);
            } // else no change
          }
          case None => {
            makeLeader(topProcess);
          }
        }
      }
    }

    ctrl uponEvent {
      case _: Start => {
        startTimer(period);
//        println(s" ${System.currentTimeMillis()} :: ${AddressUtils.toRank(self)}-${self.hashCode()} started BLE");
      }
    }

    timer uponEvent {
      case LECheckTimeout(_) => {
        checkLeader();

        round += 1;
        for (p <- topology) {
          if (p != self)
            trigger(PL_Send(p, LEHeartbeatReq(round, highestBallot)) -> pl);
        }

        ballots.clear();
        startTimer(period);
      }
    }

    pl uponEvent {
      case PL_Deliver(src, LEHeartbeatReq(r, hb)) => {
        if (hb > highestBallot) {
          highestBallot = hb;
        }
        trigger(PL_Send(src, LEHeartbeatResp(r, ballot)) -> pl);
      }
      case PL_Deliver(src, LEHeartbeatResp(r, b)) => {
        if (r == round) {
          ballots += (src -> b)
        } else {
//          println(s" ${System.currentTimeMillis()} :: ${AddressUtils.toRank(self)} received HB for ${r} in ${round}");
          period += delta;
          //        if (b > highestBallot) {
          //          highestBallot = b;
          //        }
        }
      }
    }
  }
}
