package se.kth.edx.id2203.core

import java.net.InetAddress;
import java.net.InetSocketAddress;
import se.sics.kompics.network.{ Address, Header, Msg, Transport };
import se.sics.kompics.KompicsEvent;

final class TAddress(val isa: InetSocketAddress) extends Address with Serializable {
  override def asSocket(): InetSocketAddress = isa;
  override def getIp(): InetAddress = isa.getAddress;
  override def getPort(): Int = isa.getPort;
  override def sameHostAs(other: Address): Boolean = {
    this.isa.equals(other.asSocket());
  }
  override def hashCode(): Int = {
    val addrBytes = isa.getAddress.getAddress;
    val portBytes = com.google.common.primitives.Ints.toByteArray(isa.getPort);
    val bytes = Array(109, addrBytes(0), addrBytes(1), addrBytes(2), addrBytes(3), portBytes(2), portBytes(3)); // arbitrary prime byte
    java.util.Arrays.hashCode(bytes)
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: TAddress => isa.getAddress.equals(other.isa.getAddress) && isa.getPort == other.isa.getPort
      case _ => false
    }
  }
  
  override def toString() : String = {
    s"TAddress(${isa.toString})"
  }
}

final case class THeader(src: Address, dst: Address, proto: Transport) extends Header[Address] {
  override def getDestination(): Address = dst;
  override def getProtocol(): Transport = proto;
  override def getSource(): Address = src;
}

class TMessage(header: THeader) extends Msg[Address, THeader] {
  override def getDestination(): Address = header.dst;
  override def getHeader(): THeader = header;
  override def getProtocol(): Transport = header.proto;
  override def getSource(): Address = header.src;
}
