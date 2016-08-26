//HOMEWORK 4 - Complete the Paxos implementation, defined bellow

package se.kth.edx.id2203.templates

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.templates.Paxos._
import se.kth.edx.id2203.validation.validation._
import se.sics.kompics.network._
import se.sics.kompics.sl.{Init, _}
import se.sics.kompics.{KompicsEvent, ComponentDefinition => _, Port => _}

import scala.collection.mutable.Map

//Define initialization event
object Paxos {

  case class Prepare(t: Int, ts: Int) extends KompicsEvent;
  case class PrepareAck(ts: Int, value: Option[AnyRef], pts: Int, t: Int) extends KompicsEvent;
  case class Nack(ballotNum: Int, t: Int) extends KompicsEvent;
  case class Accept(ts: Int, value: Option[AnyRef], t: Int) extends KompicsEvent;
  case class AcceptAck(ballotNum: Int, t: Int) extends KompicsEvent;

}

class Paxos(paxosInit: Init[Paxos]) extends ComponentDefinition {

  val ac = provides[AbortableConsensus];
  val beb = requires[BestEffortBroadcast];
  val pp2p = requires[PerfectLink];

  val (self, quorumSize) = paxosInit match {
    case Init(s: Address, qSize: Int) => (s, qSize)
  }

  var t = 0;
  var prepts = 0;
  var ats = 0;
  var av: Option[AnyRef] = None;

  var ballotNum = 0;
  var proposedValue: Option[AnyRef] = None;
  var readlist: Map[Address, PrepareAck] = Map.empty;
  var numAcks = 0;

  ac uponEvent {
    case AC_Propose(value) => handle {
      t = t + 1;
      ballotNum = t * quorumSize + toRank(self);
      proposedValue = Some(value);
      readlist.clear();
      numAcks = 0;
      trigger(BEB_Broadcast(Prepare(ballotNum, t)) -> beb);
    }
  }

  beb uponEvent {
    case BEB_Deliver(src, prep: Prepare) => handle {
      t = math.max(t, prep.t) + 1;

      if (prepts < prep.ts) {
        prepts = prep.ts;
        trigger(PL_Send(src, PrepareAck(ats, av, prep.ts, t)) -> pp2p);
      }
      else {
        trigger(PL_Send(src, Nack(prep.ts, t)) -> pp2p);
      }
    };

    case BEB_Deliver(src, acc: Accept) => handle {
      t = math.max(t, acc.t) + 1;

      if (prepts <= acc.ts) {
        prepts = acc.ts;
        ats = acc.ts;
        av = acc.value;
        trigger(PL_Send(src, AcceptAck(acc.ts, t)) -> pp2p);
      }
      else {
        trigger(PL_Send(src, Nack(acc.ts, t)) -> pp2p);
      }
    }
  }

  pp2p uponEvent {
    case PL_Deliver(src, prepAck: PrepareAck) => handle {
      t = math.max(t, prepAck.t) + 1;

      if (ballotNum == prepAck.pts) {
        readlist += (src -> prepAck)
        if (readlist.size > quorumSize / 2) {
          val highest = readlist.maxBy(entry => entry._2.ts)._2;
          if (highest.ts != 0) {
            proposedValue = highest.value;
          }
          readlist.clear();
          trigger(BEB_Broadcast(Accept(ballotNum, proposedValue, t)) -> beb);
        }
      }
    };

    case PL_Deliver(src, accAck: AcceptAck) => handle {
      t = math.max(t, accAck.t) + 1;
      
      if(ballotNum == accAck.ballotNum) {
        numAcks = numAcks + 1;
        if(numAcks > quorumSize / 2){
          ballotNum = 0;
          trigger(AC_Decide(proposedValue) -> ac);
        }
      }
    };

    case PL_Deliver(src, nack: Nack) => handle {
      t = math.max(t, nack.t) + 1;
      
      if(ballotNum == nack.ballotNum){
        ballotNum = 0;
        trigger(AC_Abort() -> ac);
      }
    }
  }


};

object PaxosTemplate extends App {
  checkAbortableConsensus[Paxos]();
}
