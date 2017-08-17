package se.kth.edx.id2203.validation.testers

import org.scalatest._
import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.Ports._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.language.implicitConversions

import se.kth.edx.id2203.core.ExercisePrimitives._
import se.kth.edx.id2203.validation.validateAndPrint
import se.sics.kompics.network._
import se.sics.kompics.sl.{ Init, _ }
import se.sics.kompics.{ ComponentDefinition => _, Port => _, KompicsEvent }

class LinerizabilityCheck extends FlatSpec with Matchers {

    /*     0   1   2   3   4   5   6   7   8   9   10   11   12   13   14
  OP1 :  ---------W0----------
  OP2 :  ---------------W0----------------
  OP3 :          --------R2-------
  OP4 :              --------------W2--------------
  OP5 :              -------------------R2-------------------
 
  Op6  :                                                              ----------------r0-----------
  
    /*     0   1   2   3   4   5   6   7   8   9   10   11   12   13   14
  OP1 :  ---------W0---------- ---------R1----------
  OP2 :       ---------R1---------- ---------W0----------
                              */
 TODO check edge removal matching 
   */

    val cyclicOps = Array(
        (toAddress(1), 0l, AR_Write_Request(Some(0))),
        (toAddress(1), 4l, AR_Write_Response()),
        (toAddress(2), 1l, AR_Read_Request()),
        (toAddress(2), 5l, AR_Read_Response(Some(1))),
        (toAddress(1), 6l, AR_Read_Request()),
        (toAddress(1), 10l, AR_Read_Response(Some(1))),
        (toAddress(2), 7l, AR_Write_Request(Some(0))),
        (toAddress(2), 11l, AR_Write_Response())
    )

    val testOps = Array(
        (toAddress(1), 0l, AR_Write_Request(0)),
        (toAddress(2), 0l, AR_Write_Request(0)),
        (toAddress(3), 2l, AR_Read_Request()),
        (toAddress(4), 3l, AR_Write_Request(2)),
        (toAddress(5), 3l, AR_Read_Request()),
        (toAddress(1), 5l, AR_Write_Response()),
        (toAddress(3), 6l, AR_Read_Response(Some(2))),
        (toAddress(2), 8l, AR_Write_Response()),
        (toAddress(4), 10l, AR_Write_Response()),
        (toAddress(5), 12l, AR_Read_Response(Some(2)))
    )

    val failOps = Array(
        (toAddress(1), 0l, AR_Write_Request(0)),
        (toAddress(2), 0l, AR_Write_Request(0)),
        (toAddress(3), 2l, AR_Read_Request()),
        (toAddress(4), 3l, AR_Write_Request(2)),
        (toAddress(5), 3l, AR_Read_Request()),
        (toAddress(1), 5l, AR_Write_Response()),
        (toAddress(3), 5l, AR_Read_Response(Some(2))),
        (toAddress(6), 14l, AR_Read_Request()),
        (toAddress(2), 8l, AR_Write_Response()),
        (toAddress(4), 10l, AR_Write_Response()),
        (toAddress(5), 12l, AR_Read_Response(Some(2))),
        (toAddress(6), 20l, AR_Read_Response(Some(1)))
    )

    val failOps2 = Array(
        (toAddress(1), 0l, AR_Write_Request(0)),
        (toAddress(2), 0l, AR_Write_Request(0)),
        (toAddress(3), 2l, AR_Read_Request()),
        (toAddress(4), 3l, AR_Write_Request(2)),
        (toAddress(5), 3l, AR_Read_Request()),
        (toAddress(1), 5l, AR_Write_Response()),
        (toAddress(3), 5l, AR_Read_Response(Some(2))),
        (toAddress(6), 14l, AR_Read_Request()),
        (toAddress(2), 8l, AR_Write_Response()),
        (toAddress(4), 10l, AR_Write_Response()),
        (toAddress(5), 12l, AR_Read_Response(Some(2))),
        (toAddress(6), 20l, AR_Read_Response(Some(0))),
        (toAddress(7), 21l, AR_Read_Request()),
        (toAddress(6), 22l, AR_Read_Response(Some(2)))
    )

    "ABDTester" should "pass scenarios where a linerarization exists" in {
        val valid = ABDTester.validateLinerizability(ListBuffer(testOps: _*));
        valid should be(true);
    }

    it should "fail scenarios where no linerarization exists" in {
        val valid = ABDTester.validateLinerizability(ListBuffer(failOps: _*));
        valid should be(false);
    }

    it should "fail scenarios where no linerarization exists between subsequent reads" in {
        val valid = ABDTester.validateLinerizability(ListBuffer(failOps2: _*));
        valid should be(false);
    }

    it should "fail scenarios where there are cycles" in {
        val valid = ABDTester.validateLinerizability(ListBuffer(cyclicOps: _*));
        valid should be(false);
    }

    it should "not fail on this example run" in {
        try {
            val valis = ABDTester[ReadImposeWriteConsultMajorityBuggy];
            valis should not be empty;
            validateAndPrint("NN Atomic Register", valis);
            succeed
        } catch {
            case e: Throwable => e.printStackTrace(System.err); fail
        }
    }

    it should "not fail on this example run either" in {
        try {
            val valis = ABDTester[ReadImposeWriteConsultMajority];
            valis should not be empty;
            validateAndPrint("NN Atomic Register", valis);
            succeed
        } catch {
            case e: Throwable => e.printStackTrace(System.err); fail
        }
    }
}

case class READ(rid: Int) extends KompicsEvent;
case class VALUE(rid: Int, ts: Int, wr: Int, value: Option[Any]) extends KompicsEvent;
case class WRITE(rid: Int, ts: Int, wr: Int, writeVal: Option[Any]) extends KompicsEvent;
case class ACK(rid: Int) extends KompicsEvent;

/**
  * This augments tuples with comparison operators implicitly, which you can use in your code.
  * examples: (1,2) > (1,4) yields 'false' and  (5,4) <= (7,4) yields 'true'
  */

class ReadImposeWriteConsultMajorityBuggy(init: Init[ReadImposeWriteConsultMajority]) extends ComponentDefinition {

    implicit def addComparators[A](x: A)(implicit o: math.Ordering[A]): o.Ops = o.mkOrderingOps(x);

    //subscriptions

    val nnar = provides[AtomicRegister];

    val pLink = requires(PerfectLink);
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
                    } else {
                        trigger(BEB_Broadcast(WRITE(rid, maxts + 1, selfRank, writeval)) -> beb);
                    }
                }
            }
        }
        case PL_Deliver(src, v: ACK) => handle {
            if (v.rid == rid) {
                acks = acks + 1;
                if (acks > n / 2) {
                    acks = 0;
                    if (reading) {
                        reading = false;
                        trigger(AR_Read_Response(readval) -> nnar);
                    } else {
                        trigger(AR_Write_Response() -> nnar)
                    }
                }
            }
        }
    }
}

class ReadImposeWriteConsultMajority(init: Init[ReadImposeWriteConsultMajority]) extends ComponentDefinition {

    implicit def addComparators[A](x: A)(implicit o: math.Ordering[A]): o.Ops = o.mkOrderingOps(x);

    //subscriptions

    val nnar = provides[AtomicRegister];

    val pLink = requires(PerfectLink);
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
                    } else {
                        trigger(BEB_Broadcast(WRITE(rid, maxts + 1, selfRank, writeval)) -> beb);
                    }
                }
            }
        }
        case PL_Deliver(src, v: ACK) => handle {
            if (v.rid == rid) {
                acks = acks + 1;
                if (acks > n / 2) {
                    acks = 0;
                    if (reading) {
                        reading = false;
                        trigger(AR_Read_Response(readval) -> nnar);
                    } else {
                        trigger(AR_Write_Response() -> nnar)
                    }
                }
            }
        }
    }
}