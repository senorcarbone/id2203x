package se.kth.edx.id2203.core

import java.net.{InetAddress, InetSocketAddress}

import se.kth.edx.id2203.core.ExercisePrimitives.PP2P._
import se.kth.edx.id2203.core.Ports._
import se.sics.kompics.{Init, KompicsEvent}
import se.sics.kompics.network.{Address, Network, Transport}
import se.sics.kompics.network.netty.DirectMessage
import se.sics.kompics.sl.{ComponentDefinition, _}


object ExercisePrimitives {


  object AddressUtils {

    def toAddress(x: Int): Address = {
      TAddress(new InetSocketAddress(InetAddress.getByName("192.168.2." + x.toString), 1000))
    }
    
    def toRank(addr: Address): Int = {
      return addr.getIp().getAddress()(3).toInt;
    }

  }

  final case class TAddress(isa: InetSocketAddress) extends Address {
    override def asSocket(): InetSocketAddress = isa;

    override def getIp(): InetAddress = isa.getAddress;

    override def getPort(): Int = isa.getPort;

    override def sameHostAs(other: Address): Boolean = {
      this.isa.equals(other.asSocket());
    }
  }

  object PP2P{  
    case class PP2PInit(selfAddr: Address) extends Init[PP2P];
    case class Pp2pMessage(src: Address, dest: Address, payload: KompicsEvent) extends DirectMessage(src, dest, Transport.TCP); 
  }
  
  class PP2P(pp2pInit: PP2PInit) extends ComponentDefinition {

    val pp2p = provides[PerfectLink];
    val network = requires[Network];

    val self = pp2pInit.selfAddr;

    pp2p uponEvent {
      case PL_Send(dest, payload) => handle {
        trigger(Pp2pMessage(self, dest, payload) -> network);
      }
    }

    network uponEvent {
      case Pp2pMessage(src, dest, payload) => handle {
        trigger(PL_Deliver(src, payload) -> pp2p);
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
