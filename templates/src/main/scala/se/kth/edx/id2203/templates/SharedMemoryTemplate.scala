package se.kth.edx.id2203.templates


import se.kth.edx.id2203.core.ExercisePrimitives._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.templates.ReadImposeWriteConsultMajority._
import se.kth.edx.id2203.validation._
import se.sics.kompics.network._
import se.sics.kompics.sl.{Init, _}
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}

import scala.collection.mutable.Map
import scala.language.implicitConversions


//companion object for RIWCM
object ReadImposeWriteConsultMajority {

  //events for internal use
  case class READ(rid: Int) extends KompicsEvent;
  case class VALUE(rid: Int, ts: Int, wr: Int, value: Option[Any]) extends KompicsEvent;
  case class WRITE(rid: Int, ts: Int, wr: Int, writeVal: Option[Any]) extends KompicsEvent;
  case class ACK(rid: Int) extends KompicsEvent;

  /**
    * This augments tuples with comparison operators implicitly, which you can use in your code. 
    * examples: (1,2) > (1,4) yields 'false' and  (5,4) <= (7,4) yields 'true' 
    */
  implicit def addComparators[A](x: A)(implicit o: math.Ordering[A]): o.Ops = o.mkOrderingOps(x);

}

class ReadImposeWriteConsultMajority(init: Init[ReadImposeWriteConsultMajority]) extends ComponentDefinition {

  //subscriptions

  val nnar = provides[AtomicRegister];

  val pLink = requires[PerfectLink];
  val beb = requires[BestEffortBroadcast];

  //state and initialization

  val (self: Address, n: Int, selfRank: Int) = init match {
    case Init(selfAddr: Address, n: Int) => (selfAddr, n, AddressUtils.toRank(selfAddr))
  };

  var (ts, wr) = (0, 0);
  var value: Option[Any] = None;
  var acks = 0;
  var readval: Option[Any] = None;
  var writeval: Option[Any] = None;
  var rid = 0;
  var readlist: Map[Address, (Int, Int, Option[Any])] = Map.empty
  var reading = false;

  //event handlers

  nnar uponEvent {
    case AR_Read_Request() => handle {
      rid = rid + 1;
      acks = 0;
      readlist.clear();
      reading = true;
      trigger(BEB_Broadcast(READ(rid)) -> beb)
    };
    case AR_Write_Request(wval) => handle {
      rid = rid + 1;
      writeval = Some(wval);
      acks = 0;
      readlist.clear();
      trigger(BEB_Broadcast(READ(rid)) -> beb);
    }
  }

  beb uponEvent {
    case BEB_Deliver(src, READ(readID)) => handle {
      trigger(PL_Send(src, VALUE(readID, ts, wr, value)) -> pLink);
    }
    case BEB_Deliver(src, w: WRITE) => handle {
      if ((w.ts, w.wr) > (ts, wr)) {
        ts = w.ts;
        wr = w.wr;
        value = w.writeVal;
      }
      trigger(PL_Send(src, ACK(rid)) -> pLink);
    }
  }

  pLink uponEvent {
    case PL_Deliver(src, v: VALUE) => handle {
      if (v.rid == rid) {
        readlist += (src -> (v.ts, v.wr, v.value));
        if (readlist.size > n / 2) {
          val (maxts, rr, mreadval) = readlist.values.toList.maxBy(x => (x._1, x._2));
          readval = mreadval;
          readlist.clear();
          if (reading) {
            trigger(BEB_Broadcast(WRITE(rid, maxts, rr, readval)) -> beb);
          }
          else {
            trigger(BEB_Broadcast(WRITE(rid, maxts + 1, selfRank, writeval)) -> beb);
          }
        }
      }
    }
    case PL_Deliver(src, v: ACK) => handle {
      acks = acks + 1;
      if (acks > n / 2) {
        acks = 0;
        if (reading) {
          reading = false;
          trigger(AR_Read_Response(readval) -> nnar);
        }
        else {
          trigger(AR_Write_Response() -> nnar)
        }
      }
    }
  }
}

object ARCheck extends App {
  checkNNAR[ReadImposeWriteConsultMajority]();
}