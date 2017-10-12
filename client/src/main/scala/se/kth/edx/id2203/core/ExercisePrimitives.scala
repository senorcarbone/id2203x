package se.kth.edx.id2203.core

import java.net.{InetAddress, InetSocketAddress}

import se.kth.edx.id2203.core.ExercisePrimitives.FifoP2PLink.{FifoLinkInit, FifoPayload}
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink._
import se.kth.edx.id2203.core.Ports._
import se.sics.kompics.{Init, KompicsEvent}
import se.sics.kompics.network.{Address, Network, Transport}
import se.sics.kompics.sl.{ComponentDefinition, _}
import se.kth.edx.id2203.core._

import scala.collection.mutable

object ExercisePrimitives {

  object AddressUtils {

    def toAddress(x: Int): Address = {
      new TAddress(new InetSocketAddress(InetAddress.getByName("192.168.2." + x.toString), 1000))
    }

    def toRank(addr: Address): Int = {
      return addr.getIp().getAddress()(3).toInt;
    }

  }

  object PerfectP2PLink {
    case class PerfectLinkInit(selfAddr: Address) extends Init[PerfectP2PLink];
    case class PerfectLinkMessage(src: Address, dest: Address, payload: KompicsEvent) extends TMessage(THeader(src, dest, Transport.TCP));
  }

  class PerfectP2PLink(pp2pInit: PerfectLinkInit) extends ComponentDefinition {

    val pLink = provides[PerfectLink];
    val network = requires[Network];

    val self = pp2pInit.selfAddr;

    pLink uponEvent {
      case PL_Send(dest, payload) => handle {
        trigger(PerfectLinkMessage(self, dest, payload) -> network);
      }
    }

    network uponEvent {
      case PerfectLinkMessage(src, dest, payload) => handle {
        trigger(PL_Deliver(src, payload) -> pLink);
      }
    }

  }                     
  
  object FifoP2PLink {
    case class FifoLinkInit(self: Address, topology: Set[Address]) extends Init[FifoP2PLink];
    case class FifoPayload(payload: KompicsEvent, seq: Int) extends KompicsEvent
  }
  
  class FifoP2PLink(fifop2pInit: FifoLinkInit) extends ComponentDefinition {
    
    val fifoLink = provides[FIFOPerfectLink];
    val pLink = requires[PerfectLink]; 
    
    val self = fifop2pInit.self;
    val topology = fifop2pInit.topology;
    
    val lsn  = collection.mutable.Map[Address, Int]().withDefaultValue(0);
    val next = collection.mutable.Map[Address, Int]().withDefaultValue(1);
    val pending = collection.mutable.Map[Address, mutable.Set[(Int, KompicsEvent)]]().withDefaultValue(mutable.Set.empty);
    
    fifoLink uponEvent {
      case PL_Send(dest, payload) => handle {
        lsn += dest -> (lsn(dest) + 1);
        trigger(PL_Send(dest, FifoPayload(payload, lsn(dest))) -> pLink);
      }
    }
    
    pLink uponEvent {
      case PL_Deliver(p, FifoPayload(payload, num)) => handle {
        pending(p) += ((num, payload))
        
        while(pending(p) map(_._1) contains next(p) ){
          val toDeliver = (pending(p) filter (_._1 == next(p)) head)
          next(p) += 1;
          pending(p) -= toDeliver
          trigger(PL_Deliver(p, toDeliver._2) -> fifoLink);
        }
      }
    }
    
  }

  case class VectorClock(var vc: Map[Address, Int]) {

    def inc(addr: Address) = {
      vc = vc + ((addr, vc.get(addr).get + 1));
    }

    def set(addr: Address, value: Int) = {
      vc = vc + ((addr, value));
    }

    def <=(that: VectorClock): Boolean = vc.foldLeft[Boolean](true)((leq, entry) => leq & (entry._2 <= that.vc.getOrElse(entry._1, entry._2)))

  }

  object VectorClock {

    def empty(topology: scala.Seq[Address]): VectorClock = {
      VectorClock(topology.foldLeft[Map[Address, Int]](Map[Address, Int]())((mp, addr) => mp + ((addr, 0))))
    }

    def apply(that: VectorClock): VectorClock = {
      VectorClock(that.vc);
    }

  }

}
