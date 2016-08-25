package se.kth.edx.id2203.templates


import se.kth.edx.id2203.core.ExercisePrimitives._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.templates.ReadImposeWriteConsultMajority._
import se.sics.kompics.network._
import se.sics.kompics.sl.{Init, _}
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}
import scala.collection.mutable.Map


//companion object for RIWCM
object ReadImposeWriteConsultMajority{
  
  //events for internal use
  case class READ(rid: Int) extends KompicsEvent;
  case class VALUE(rid: Int, ts: Int, wr: Int, value : Option[AnyRef]) extends KompicsEvent;
  case class WRITE(rid: Int, ts: Int, wr: Int, writeVal: Option[AnyRef]) extends KompicsEvent;
  case class ACK(rid: Int) extends KompicsEvent;
  
  /**
    * This augments tuples with comparison operators implicitly, which you can use in your code. 
    * examples: (1,2) > (1,4) yields 'false' and  (5,4) <= (7,4) yields 'true' 
    */
  implicit def addComparators[A](x: A)(implicit o: math.Ordering[A]): o.Ops = o.mkOrderingOps(x)
  
}

class ReadImposeWriteConsultMajority(init: Init[ReadImposeWriteConsultMajority]) extends ComponentDefinition {
  
  //subscriptions
  
  val nnar = provides[AtomicRegister];
  
  val pp2p = requires[PerfectP2PLink];
  val beb = requires[BestEffortBroadcast];

  //state and initialization

  val (self, n, selfRank) = init match { 
    case Init(selfAddr: Address, n: Int) => (self, n, AddressUtils.toRank(self)) 
  };
  
  var (ts, wr, value: Option[AnyRef]) = (0,0, None);
  var acks = 0;
  var readval : Option[AnyRef] = None;
  var writeval : Option[AnyRef] = None;
  var rid = 0;
  var readlist : Map[Address, (Int, Int, Option[AnyRef])] =  Map.empty
  var reading = false;
  
  //event handlers
  
  nnar uponEvent {
    case AR_READ_REQUEST() => handle {
      rid = rid + 1;
      acks = 0;
      readlist = Map.empty;
      reading = true;
      trigger(BEB_Broadcast(READ(rid)) -> beb)
    };
    case AR_WRITE_REQUEST(wval) => handle {
      rid = rid + 1;
      writeval = wval;
      acks = 0;
      readlist = Map.empty;
      trigger(READ(rid) -> beb);
    } 
  }
  
  beb uponEvent {
    case BEB_Deliver(src, READ(readID)) => handle {
      trigger(Pp2pSend(src, VALUE(readID, ts, wr, value)) -> pp2p);
    }
    case BEB_Deliver(src, w: WRITE) => handle {
      if ((w.ts, w.wr) > (ts, wr)) {
        (ts, wr, value) = (w.ts, w.wr, w.writeVal);
      } 
      trigger(Pp2pSend(src, ACK(rid)) -> pp2p);
    }
  }
  
  pp2p uponEvent {
    case Pp2pDeliver(src, v: VALUE) => handle {
      if(v.rid == rid){
        readlist += (src -> (v.ts,v.wr,v.value));
        if(readlist.size > n/2){
          val (maxts, rr, mreadval) = readlist.values.toList.max;
          readlist = Map.empty;
          if(reading){
            trigger(BEB_Broadcast(WRITE(rid, maxts, rr, mreadval)) -> beb);
          }
          else {
            trigger(BEB_Broadcast(WRITE(rid, maxts+1, selfRank, writeval)) -> beb);
          }
        }
      }
    }
    case Pp2pDeliver(src, v: ACK) => handle {
      acks = acks + 1;
      if(acks > n/2){
        acks = 0;
        if(reading){
          reading = false;
          trigger(AR_READ_RESPONSE(readval) -> nnar);
        }
        else {
          trigger(AR_WRITE_RESPONSE() -> nnar)
        }
      }
    }
  }
  
  
  

}