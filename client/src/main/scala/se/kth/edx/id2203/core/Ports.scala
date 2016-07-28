package se.kth.edx.id2203.core

import se.sics.kompics.KompicsEvent
import se.sics.kompics.network.Address
import se.sics.kompics.sl._

object Ports {

  case class Pp2pDeliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

  case class Pp2pSend(dest: Address, payload: KompicsEvent) extends KompicsEvent;

  class PerfectP2PLink extends Port {
    indication[Pp2pDeliver];
    request[Pp2pSend];
  }

  case class Suspect(src: Address) extends KompicsEvent;

  case class Restore(src: Address) extends KompicsEvent;

  class EventuallyPerfectFailureDetector extends Port {
    indication[Suspect];
    indication[Restore];
  }

  case class BEB_Deliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

  case class BEB_Broadcast(payload: KompicsEvent) extends KompicsEvent;

  class BestEffortBroadcast extends Port {
    indication[BEB_Deliver];
    request[BEB_Broadcast];
  }

  case class RB_Deliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

  case class RB_Broadcast(payload: KompicsEvent) extends KompicsEvent;

  class ReliableBroadcast extends Port {
    indication[RB_Deliver];
    request[RB_Broadcast];
  }

  case class CRB_Deliver(src: Address, payload: KompicsEvent) extends KompicsEvent;

  case class CRB_Broadcast(payload: KompicsEvent) extends KompicsEvent;

  class ReliableCausalOrderBroadcast extends Port {
    indication[CRB_Deliver];
    request[CRB_Broadcast];
  }

}
