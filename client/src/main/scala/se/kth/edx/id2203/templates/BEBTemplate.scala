////HOMEWORK 2 - Implement Causal Order Reliable Broadcast, defined bellow
//
package se.kth.edx.id2203.templates

import ExercisePrimitives._
import se.sics.kompics.network._
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.sl._
import se.sics.kompics.sl.simulator.Distributions
import se.sics.kompics.timer.Timer
import se.sics.kompics.{Init, ComponentDefinition => _, Port => _, _}

import scala.collection.mutable
import scala.collection.mutable._

//Best Effort Broadcast

case class BEB_Deliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

case class BEB_Broadcast(payload: KompicsEvent) extends KompicsEvent;

class BestEffortBroadcast extends Port {
  indication[BEB_Deliver];
  request[BEB_Broadcast];
}

case class BasicBroadcastInit(selfAddr: Address, topology: Set[Address]) extends Init[BasicBroadcast];

class BasicBroadcast(init: BasicBroadcastInit) extends ComponentDefinition {

  //BasicBroadcast Subscriptions
  val pp2p = requires[PerfectP2PLink];
  val beb = provides[BestEffortBroadcast];

  //BasicBroadcast Component State and Initialization
  val self = init.selfAddr;
  val topology = init.topology;

  //BasicBroadcast Event Handlers
  beb uponEvent {
    case x: BEB_Broadcast => handle {
      topology.foreach(addr => trigger(Pp2pSend(addr, x) -> pp2p));
    }
  }

  pp2p uponEvent {
    case Pp2pDeliver(src, BEB_Broadcast(payload)) => handle {
      trigger(BEB_Deliver(src, payload) -> beb);
    }
  }
}

//Reliable Broadcast

case class RB_Deliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

case class RB_Broadcast(payload: KompicsEvent) extends KompicsEvent;

class ReliableBroadcast extends Port {
  indication[RB_Deliver];
  request[RB_Broadcast];
}

case class EagerReliableBroadcastInit(selfAddr: Address) extends Init[EagerReliableBroadcast];

class EagerReliableBroadcast(init: EagerReliableBroadcastInit) extends ComponentDefinition {
  //EagerReliableBroadcast Subscriptions
  val beb = requires[BestEffortBroadcast];
  val rb = provides[ReliableBroadcast];

  //EagerReliableBroadcast Component State and Initialization
  val self = init.selfAddr;
  val delivered = mutable.Set[KompicsEvent]();

  //EagerReliableBroadcast Event Handlers
  rb uponEvent {
    case x:RB_Broadcast => handle {
      trigger(BEB_Broadcast(x) -> beb);
    }
  }

  beb uponEvent {
    case BEB_Deliver(src, y@RB_Broadcast(payload)) => handle {
      if (!delivered.contains(payload)) {
        delivered += payload;
        trigger(RB_Deliver(src, payload) -> rb);
        trigger(BEB_Broadcast(y) -> beb);
      }
    }
  }
  
}

//Causal Reliable Broadcast

case class CRB_Deliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

case class CRB_Broadcast(payload: KompicsEvent) extends KompicsEvent;

class ReliableCausalOrderBroadcast extends Port {
  indication[CRB_Deliver];
  request[CRB_Broadcast];
}

case class WaitingCRBInit(selfAddr: Address, topology: Set[Address]) extends Init[WaitingCRB];

class WaitingCRB(init: WaitingCRBInit) extends ComponentDefinition {

  //WaitingCRB Subscriptions
  val rb = requires[ReliableBroadcast];
  val crb = provides[ReliableCausalOrderBroadcast];

  //WaitingCRB Component State and Initialization
  val self = init.selfAddr;
  val V = VectorClock.empty(init.topology.toSeq)
  var pending :ListBuffer[(Address, DataMessage)] = ListBuffer();
  var lsn = 0;

  //Declare custom message types related to internal component implementation
  case class DataMessage(timestamp: VectorClock, payload: KompicsEvent) extends KompicsEvent;
  
  //WaitingCRB Event Handlers
  crb uponEvent {
    case x: CRB_Broadcast => handle {
      val W = VectorClock(V);
      W.set(self, lsn);
      lsn = lsn + 1;
      trigger(RB_Broadcast(DataMessage(W, x.payload)) -> rb);
    }
  }

  rb uponEvent {
    case x@RB_Deliver(src:Address, msg:DataMessage) => handle {
      pending += (src -> msg);
      val toDeliver = pending.filter {case (_, data) => data.timestamp <= V}
      toDeliver.foreach { case tpl@(addr:Address, data:DataMessage) =>
        pending -= tpl;
        V.inc(addr);
        trigger(CRB_Deliver(addr, data.payload) -> crb)
      };
    }
  }
}


// - Program interfaces for Simulation
case class CRBIntercInit(self: Address) extends Init[CRBInterceptor];

class CRBInterceptor(init: CRBIntercInit) extends ComponentDefinition {
  
  val self = init.self;
  val crb = requires[ReliableCausalOrderBroadcast];

  crb uponEvent {
    case CRB_Deliver(src,payload) => handle {
      //TODO
    }
  }

}

case class CRBParentInit(selfAddr: Address, topology: Set[Address]) extends Init[EPFDParent];

class CRBParent(init: CRBParentInit) extends ComponentDefinition {

  val network: PositivePort[Network] = requires[Network];
  val timer = requires[Timer];
  val pp2p = create(classOf[PP2P], PP2PInit(init.selfAddr));
  
  val beb = create(classOf[BasicBroadcast], BasicBroadcastInit(init.selfAddr, init.topology));
  val rb = create(classOf[EagerReliableBroadcast], EagerReliableBroadcastInit(init.selfAddr));
  val crb = create(classOf[WaitingCRB], WaitingCRBInit(init.selfAddr, init.topology));
  
  val interceptor = create(classOf[EPFDInterceptor], EPFDIntercInit(init.selfAddr));

  connect[Network](network -> pp2p);
  connect[PerfectP2PLink](pp2p -> beb);
  connect[BestEffortBroadcast](beb -> rb);
  connect[ReliableBroadcast](rb -> crb);
  connect[ReliableCausalOrderBroadcast](crb -> interceptor);
  
}


case class CRBValidation(numNodes: Int) {
  
  import Distributions._

  val topology = (1 to numNodes).map(AddressUtils.toAddress(_)).toList;
  
  
  
  
}




