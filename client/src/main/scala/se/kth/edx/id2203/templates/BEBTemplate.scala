////HOMEWORK 2 - Implement Causal Order Reliable Broadcast, defined bellow

package se.kth.edx.id2203.templates

import se.kth.edx.id2203.core.ExercisePrimitives._
import se.kth.edx.id2203.core.Ports._
import se.sics.kompics.network._
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.sl.{Init, _}
import se.sics.kompics.timer.Timer
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}

import scala.collection.mutable
import scala.collection.mutable._

//Best Effort Broadcast

class BasicBroadcast(init: Init[BasicBroadcast]) extends ComponentDefinition {

  //BasicBroadcast Subscriptions
  val pp2p = requires[PerfectP2PLink];
  val beb = provides[BestEffortBroadcast];

  //BasicBroadcast Component State and Initialization
  val (self, topology) = init match { case Init(s: Address, t: Set[Address] @unchecked) => (s, t) };

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

class EagerReliableBroadcast(init: Init[EagerReliableBroadcast]) extends ComponentDefinition {
  //EagerReliableBroadcast Subscriptions
  val beb = requires[BestEffortBroadcast];
  val rb = provides[ReliableBroadcast];

  //EagerReliableBroadcast Component State and Initialization
  val self = init match {case Init(s: Address) => s};
  val delivered = mutable.Set[KompicsEvent]();

  //EagerReliableBroadcast Event Handlers
  rb uponEvent {
    case x@RB_Broadcast(_) => handle {
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


class WaitingCRB(init: Init[WaitingCRB]) extends ComponentDefinition {

  //WaitingCRB Subscriptions
  val rb = requires[ReliableBroadcast];
  val crb = provides[ReliableCausalOrderBroadcast];

  //WaitingCRB Component State and Initialization
  val (self, vec) = init match { case Init(s: Address, t: Set[Address] @unchecked) => (s,  VectorClock.empty(t.toSeq)) };
 
//  val V = VectorClock.empty(init match { case Init(_, t: Set[Address]) => t.toSeq })
  var pending: ListBuffer[(Address, DataMessage)] = ListBuffer();
  var lsn = 0;

  //Declare custom message types related to internal component implementation
  case class DataMessage(timestamp: VectorClock, payload: KompicsEvent) extends KompicsEvent;

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
    case x@RB_Deliver(src: Address, msg: DataMessage) => handle {
      pending += (src -> msg);
      val toDeliver = pending.filter { case (_, data) => data.timestamp <= vec}
      toDeliver.foreach { case tpl@(addr: Address, data: DataMessage) =>
        pending -= tpl;
        vec.inc(addr);
        trigger(CRB_Deliver(addr, data.payload) -> crb)
      };
    }
  }
}


// - Program interfaces for Simulation

class CRBInterceptor(init: Init[CRBInterceptor]) extends ComponentDefinition {

  val self = init match { case Init(s: Address) => s};
  val crb = requires[ReliableCausalOrderBroadcast];

  crb uponEvent {
    case CRB_Deliver(src, payload) => handle {
      //TODO
    }
  }

}

class CRBParent(init: Init[CRBParent]) extends ComponentDefinition {

  val (selfAddr, topology) = init match { case Init(s: Address, t: Set[Address] @unchecked) => (s, t)};

  val network: PositivePort[Network] = requires[Network];
  val timer = requires[Timer];
  val pp2p = create(classOf[PP2P], PP2PInit(selfAddr));

  val beb = create(classOf[BasicBroadcast], Init[BasicBroadcast](selfAddr, topology));
  val rb = create(classOf[EagerReliableBroadcast], Init[EagerReliableBroadcast](selfAddr));
  val crb = create(classOf[WaitingCRB], Init[WaitingCRB](selfAddr, topology));

  val interceptor = create(classOf[CRBInterceptor], Init[CRBInterceptor](selfAddr));

  connect[Network](network -> pp2p);
  connect[PerfectP2PLink](pp2p -> beb);
  connect[BestEffortBroadcast](beb -> rb);
  connect[ReliableBroadcast](rb -> crb);
  connect[ReliableCausalOrderBroadcast](crb -> interceptor);

}


case class CRBValidation(numNodes: Int) {

  val topology = (1 to numNodes).map(AddressUtils.toAddress(_)).toList;
  //FIXME


}


object BEBTemplate extends App  {
  val seed = 123;
  JSimulationScenario.setSeed(seed);
//  ValidationSimulation.runAndValidate(BEBTemplate.scenario);
}





