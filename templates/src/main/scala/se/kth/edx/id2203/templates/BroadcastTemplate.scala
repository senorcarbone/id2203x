////HOMEWORK 2 - Implement Causal Order Reliable Broadcast, defined bellow

package se.kth.edx.id2203.templates

import se.kth.edx.id2203.core.ExercisePrimitives._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.templates.WaitingCRB._
import se.kth.edx.id2203.validation._
import se.sics.kompics.network._
import se.sics.kompics.sl.{ Init, _ }
import se.sics.kompics.{ ComponentDefinition => _, Port => _, _ }

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable._

//Best Effort Broadcast

class BasicBroadcast(init: Init[BasicBroadcast]) extends ComponentDefinition {

  //BasicBroadcast Subscriptions
  val pLink = requires[PerfectLink];
  val beb = provides[BestEffortBroadcast];

  //BasicBroadcast Component State and Initialization
  val (self, topology) = init match {
    case Init(s: Address, t: Set[Address] @unchecked) => (s, t)
  };

  //BasicBroadcast Event Handlers
  beb uponEvent {
    case x: BEB_Broadcast => handle {
      topology.foreach(addr => trigger(PL_Send(addr, x) -> pLink));
    }
  }

  pLink uponEvent {
    case PL_Deliver(src, BEB_Broadcast(payload)) => handle {
      trigger(BEB_Deliver(src, payload) -> beb);
    }
  }
}

//object BebCheck extends App  {
//  checkBEB[BasicBroadcast]();
//}

//Reliable Broadcast

class EagerReliableBroadcast(init: Init[EagerReliableBroadcast]) extends ComponentDefinition {
  //EagerReliableBroadcast Subscriptions
  val beb = requires[BestEffortBroadcast];
  val rb = provides[ReliableBroadcast];

  //EagerReliableBroadcast Component State and Initialization
  val self = init match {
    case Init(s: Address) => s
  };
  val delivered = mutable.Set[KompicsEvent]();

  //EagerReliableBroadcast Event Handlers
  rb uponEvent {
    case x @ RB_Broadcast(_) => handle {
      trigger(BEB_Broadcast(x) -> beb);
    }
  }

  beb uponEvent {
    case BEB_Deliver(src, y @ RB_Broadcast(payload)) => handle {
      if (!delivered.contains(payload)) {
        delivered += payload;
        trigger(RB_Deliver(src, payload) -> rb);
        trigger(BEB_Broadcast(y) -> beb);
      }
    }
  }
}

//object RBCheck extends App  {
//  checkRB[BasicBroadcast,EagerReliableBroadcast]();
//}

//Causal Reliable Broadcast

//Declare custom message types related to internal component implementation

object WaitingCRB {

  case class DataMessage(timestamp: VectorClock, payload: KompicsEvent) extends KompicsEvent;
}

class WaitingCRB(init: Init[WaitingCRB]) extends ComponentDefinition {

  //WaitingCRB Subscriptions
  val rb = requires[ReliableBroadcast];
  val crb = provides[CausalOrderReliableBroadcast];

  //WaitingCRB Component State and Initialization
  val (self, vec) = init match {
    case Init(s: Address, t: Set[Address] @unchecked) => (s, VectorClock.empty(t.toSeq))
  };

  //  val V = VectorClock.empty(init match { case Init(_, t: Set[Address]) => t.toSeq })
  var pending: ListBuffer[(Address, DataMessage)] = ListBuffer();
  var lsn = 0;

  //WaitingCRB Event Handlers
  crb uponEvent {
    case x: CRB_Broadcast => handle {
      val W = VectorClock(vec);
      W.set(self, lsn);
      lsn = lsn + 1;
      trigger(RB_Broadcast(DataMessage(W, x.payload)) -> rb);
    }
  }

  rb uponEvent {
    case x @ RB_Deliver(src: Address, msg: DataMessage) => handle {
      pending += (src -> msg);

      while (pending.exists(_._2.timestamp <= vec)) {
        pending.filter { case (_, data) => data.timestamp <= vec }.foreach {
          case tpl @ (addr: Address, DataMessage(_,payload)) =>
            pending -= tpl;
            vec.inc(addr);
            trigger(CRB_Deliver(addr, payload) -> crb)
        };
      }
    }
  }
}

//object CRBCheck extends App {
//  checkCRB[BasicBroadcast, EagerReliableBroadcast, WaitingCRB]();
//}

