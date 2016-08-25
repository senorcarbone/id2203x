package se.kth.edx.id2203.templates


import se.kth.edx.id2203.core.ExercisePrimitives._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.templates.WaitingCRB._
import se.kth.edx.id2203.validation.validation._
import se.sics.kompics.network._
import se.sics.kompics.sl.{Init, _}
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable._


class ReadImposeWriteConsultMajority(init: Init[ReadImposeWriteConsultMajority]) extends ComponentDefinition {
  
  //subscriptions
  
  val nnar = provides[AtomicRegister];
  
  val pp2p = requires[PerfectP2PLink];
  val beb = requires[BestEffortBroadcast];

  //state and initialization

  val N = init match { case Init(n: Int) => n };
  
  var (ts, wr, value: Option[AnyRef]) = (0,0, None);
  var acks = 0;
  var writeval : Option[AnyRef] = None;
  var rid = 0;
  var readlist = new Array[Option[AnyRef]](N);
  var readval : Option[AnyRef] = None;
  var reading = false;
  
//  nnar uponEvent {
//    case AR_READ_REQUEST
//  }
//  
  
  
  
}