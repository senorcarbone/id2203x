//HOMEWORK 4 - Complete the Paxos implementation, defined bellow

package se.kth.edx.id2203.templates

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.templates.Paxos._
import se.kth.edx.id2203.validation._
import se.sics.kompics.network._
import se.sics.kompics.sl.{ Init, _ }
import se.sics.kompics.{ KompicsEvent, ComponentDefinition => _, Port => _ }
import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer;

//Internal messages used by the Paxos protocol
object Paxos {

  case class Prepare(proposalBallot: (Int, Int)) extends KompicsEvent;
  case class Promise(promiseBallot: (Int, Int), acceptedBallot: (Int, Int), acceptedValue: Option[Any]) extends KompicsEvent;
  case class Accept(acceptBallot: (Int, Int), proposedValue: Any) extends KompicsEvent;
  case class Accepted(acceptedBallot: (Int, Int)) extends KompicsEvent;
  case class Nack(ballot: (Int, Int)) extends KompicsEvent;
  case class Decided(decidedValue: Any) extends KompicsEvent;

  /**
   * This augments tuples with comparison operators implicitly, which you can use in your code, for convenience.
   * examples: (1,2) > (1,4) yields 'false' and  (5,4) <= (7,4) yields 'true'
   */
  implicit def addComparators[A](x: A)(implicit o: math.Ordering[A]): o.OrderingOps = o.mkOrderingOps(x);

}

class Paxos(paxosInit: Init[Paxos]) extends ComponentDefinition {

  //Port Subscriptions for Paxos

  val consensus = provides[Consensus];
  val beb = requires[BestEffortBroadcast];
  val plink = requires[PerfectLink];

  //Internal State of Paxos

  val (rank, numProcesses) = paxosInit match {
    case Init(s: Address, qSize: Int) => (toRank(s), qSize)
  }

  //Proposer State
  var round = 0;
  var proposedValue: Option[Any] = None;
  var promises: ListBuffer[((Int, Int), Option[Any])] = ListBuffer.empty;
  var numOfAccepts = 0;
  var decided = false;

  //Acceptor State
  var promisedBallot = (0, 0);
  var acceptedBallot = (0, 0);
  var acceptedValue: Option[Any] = None;

  def propose() = {
    if (!decided) {
      round = round + 1;
      numOfAccepts = 0;
      promises.clear();
      trigger(BEB_Broadcast(Prepare((round, rank))) -> beb);
    }
  }

  consensus uponEvent {
    case C_Propose(value) => {
      proposedValue = Some(value);
      propose();
    }
  }

  beb uponEvent {

    case BEB_Deliver(src, prep: Prepare) => {
      if (promisedBallot < prep.proposalBallot) {
        promisedBallot = prep.proposalBallot;
        trigger(PL_Send(src, Promise(promisedBallot, acceptedBallot, acceptedValue)) -> plink);
      } else {
        trigger(PL_Send(src, Nack(prep.proposalBallot)) -> plink);
      }
    };

    case BEB_Deliver(src, acc: Accept) => {

      if (promisedBallot <= acc.acceptBallot) {

        promisedBallot = acc.acceptBallot;
        acceptedBallot = acc.acceptBallot;
        acceptedValue = Some(acc.proposedValue);
        trigger(PL_Send(src, Accepted(acc.acceptBallot)) -> plink);
      } else {
        trigger(PL_Send(src, Nack(acc.acceptBallot)) -> plink);
      }
    };

    case BEB_Deliver(src, dec: Decided) => {
      if (!decided) {
        trigger(C_Decide(dec.decidedValue) -> consensus);
        decided = true;
      }
    }
  }

  plink uponEvent {

    case PL_Deliver(src, prepAck: Promise) => {
      if ((round, rank) == prepAck.promiseBallot) {
        promises += (prepAck.acceptedBallot -> prepAck.acceptedValue);
        if (promises.size > (numProcesses + 1) / 2) {
          proposedValue = Some(promises.maxBy(entry => entry._1)._2.getOrElse(proposedValue.get));
          trigger(BEB_Broadcast(Accept((round, rank), proposedValue.get)) -> beb);
        }
      }
    };

    case PL_Deliver(src, accAck: Accepted) => {
      if (promisedBallot == accAck.acceptedBallot) {
        numOfAccepts = numOfAccepts + 1;
        if (numOfAccepts == (numProcesses + 1) / 2) {
          trigger(BEB_Broadcast(Decided(proposedValue)) -> beb);
        }
      }
    };

    case PL_Deliver(src, nack: Nack) => {

      if ((round, rank) == nack.ballot) {
        propose(); // retry
      }
    }
  }

};

object PaxosTemplate extends App {
  checkSingleValueConsensus[Paxos]();
}
