package se.kth.edx.id2203.core

import java.net.InetAddress;
import java.net.InetSocketAddress;
import se.sics.kompics.network.{ Address, Header, Msg, Transport };
import se.sics.kompics.KompicsEvent;

final case class TAddress(isa: InetSocketAddress) extends Address {
  override def asSocket(): InetSocketAddress = isa;
  override def getIp(): InetAddress = isa.getAddress;
  override def getPort(): Int = isa.getPort;
  override def sameHostAs(other: Address): Boolean = {
    this.isa.equals(other.asSocket());
  }
  override def hashCode(): Int = {
    val addrBytes = isa.getAddress.getAddress;
    val portBytes = com.google.common.primitives.Ints.toByteArray(isa.getPort);
    val bytes = Array(109, addrBytes(0), addrBytes(1), addrBytes(2), addrBytes(3), portBytes(3), addrBytes(4)); // arbitrary prime byte
    java.util.Arrays.hashCode(bytes)
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
