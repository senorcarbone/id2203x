package se.kth.edx.id2203.validation.testers.TesterPrimitives

import se.kth.edx.id2203.core.Ports._
import se.sics.kompics.network.Address
import se.sics.kompics.sl.{ComponentDefinition, _}

import scala.collection.immutable.Set

object TesterPrimitives {

  class ProvidedBroadcast(init: Init[ProvidedBroadcast]) extends ComponentDefinition {
    val pp2p = requires[PerfectLink];
    val beb = provides[BestEffortBroadcast];
    val (self, topology) = init match {
      case Init(s: Address, t: Set[Address]@unchecked) => (s, t)
    };
    beb uponEvent {
      case x: BEB_Broadcast => handle {
        topology.foreach(addr => trigger(PL_Send(addr, x) -> pp2p));
      }
    }
    pp2p uponEvent {
      case PL_Deliver(src, BEB_Broadcast(payload)) => handle {
        trigger(BEB_Deliver(src, payload) -> beb);
      }
    }
  }

}
