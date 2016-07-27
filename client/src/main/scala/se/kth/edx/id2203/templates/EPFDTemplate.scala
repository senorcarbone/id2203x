//HOMEWORK 1 - Implement EPFD, defined bellow

package se.kth.edx.id2203.templates

import ExercisePrimitives._
import se.kth.edx.id2203.validation.validation.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.sics.kompics.network._
import se.sics.kompics.simulator.network.identifier.Identifier
import se.sics.kompics.simulator.network.identifier.impl.SocketIdExtractor
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.sl._
import se.sics.kompics.sl.simulator._
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{Init, ComponentDefinition => _, Port => _, _}

import scala.collection.JavaConversions._
import scala.collection.mutable._
import scala.concurrent.duration._


case class Suspect(src: Address) extends KompicsEvent;
case class Restore(src: Address) extends KompicsEvent;

class EventuallyPerfectFailureDetector extends Port {
  indication[Suspect];
  indication[Restore];
}

//Implementation Skeleton-------------

//Define initialization event
case class EPFDInit(selfAddr: Address) extends Init[EPFD];

//Define EPFD Implementation
class EPFD(epfdInit: EPFDInit) extends ComponentDefinition {

  //EPFD subscriptions
  val timer = requires[Timer];
  val pp2p = requires[PerfectP2PLink];
  val epfd = provides[EventuallyPerfectFailureDetector];

  // EPDF component state and initialization
  val self = epfdInit.selfAddr;
  val topology = cfg.getValue[List[Address]]("epfd.simulation.topology");
  val delta = cfg.getValue[Long]("epfd.simulation.delay");

  var period = cfg.getValue[Long]("epfd.simulation.delay");
  var alive = Set(cfg.getValue[List[Address]]("epfd.simulation.topology"): _*);
  var suspected = Set[Address]();

  //Declare custom message types related to internal component implementation
  case class CheckTimeout(timeout: ScheduleTimeout) extends Timeout(timeout);

  case class Heartbeat() extends KompicsEvent;


  def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period);
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout));
    trigger(scheduledTimeout -> timer);
  }

  //EPFD event handlers
  ctrl uponEvent {
    case _: Start => handle {
      println("EPFD Started");
      startTimer(period);
    }
  }

  timer uponEvent {
    case CheckTimeout(_) => handle {
      if (!alive.intersect(suspected).isEmpty) {
        period = period + delta;
      }

      for (p <- topology) {
        if (!alive.contains(p) && !suspected.contains(p)) {
          suspected = suspected + p;
          trigger(Suspect(p) -> epfd);
        } else if (alive.contains(p) && suspected.contains(p)) {
          suspected = suspected - p;
          trigger(Restore(p) -> epfd);
        }
        trigger(Pp2pSend(p, Heartbeat()) -> pp2p);
      }

      alive = Set[Address]();
      startTimer(period);
    }
  }

  pp2p uponEvent {
    case Pp2pDeliver(src, Heartbeat()) => handle {
      //      println(s"Heartbeat received from  $src. to $self . Adding it to the list");
      alive = alive + src;
    }
  }

};

//--PROGRAM INTERFACES FOR SIMULATOR

case class EPFDIntercInit(self: Address) extends Init[EPFDInterceptor];

class EPFDInterceptor(init: EPFDIntercInit) extends ComponentDefinition {

  val self = init.self;
  val epfd = requires[EventuallyPerfectFailureDetector];

  epfd uponEvent {
    case Restore(addr) => handle {
      //FIXME
      println(s"Restore:$self -> $addr :: ${System.currentTimeMillis()}");
      //TODO update global state here
    }
    case Suspect(addr) => handle {
      //FIXME
      println(s"Suspect:$self -> $addr :: ${System.currentTimeMillis()}");
    }
  }

  def logSuspect(): Unit = {
    val gv = cfg.getValue[GlobalView]("simulation.globalview");
    gv.setValue("simulation.suspects", gv.getValue("simulation.suspects", classOf[Integer]) + 1);
  }

}

case class EPFDParentInit(selfAddr: Address) extends Init[EPFDParent];

class EPFDParent(init: EPFDParentInit) extends ComponentDefinition {

  val network: PositivePort[Network] = requires[Network];
  val timer = requires[Timer];
  val pp2p = create(classOf[PP2P], PP2PInit(init.selfAddr))
  val epfd = create(classOf[EPFD], EPFDInit(init.selfAddr));

  val interceptor = create(classOf[EPFDInterceptor], EPFDIntercInit(init.selfAddr));

  connect[Network](network -> pp2p);
  connect[PerfectP2PLink](pp2p -> epfd);
  connect[Timer](timer -> epfd);
  connect[EventuallyPerfectFailureDetector](epfd -> interceptor);

}

// // Uncomment EPFDHost to bind to physical execution 

//class EPFDHost extends ComponentDefinition {
//
//  val self = cfg.getValue[Address]("epfd.self");
//
//  val network = create(classOf[NettyNetwork], new NettyInit(self));
//  val timer = create(classOf[JavaTimer], Init.NONE);
//  val epfdParent = create(classOf[EPFDParent], Init.NONE);
//
//  connect(epfdParent.getNegative(classOf[Network]), network.getPositive(classOf[Network]), Channel.TWO_WAY);
//  connect(epfdParent.getNegative(classOf[Timer]), network.getPositive(classOf[Timer]), Channel.TWO_WAY);
//
//}

//--SIMULATION CODE STARTS HERE

case class EPFDValidation(numNodes: Int, discNodes: Int, delay: Long, delta: Long, maxLinkDelay: Long) {

  import Distributions._

  val topology = (1 to numNodes).map(AddressUtils.toAddress(_)).toList;
  
  //FIXME
  val prop : ValidationGroup = Property("Eventual Correctness")
    .addValidator(SimValidator((_.getDeadNodes.size() == 2),15000, "2 nodes are killed", "no correct nodes are killed")).build();
  
  
  //Simulation Operations ----

  val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue("simulation.suspects", 0)).build());

  val setNormalLossyNetwork = () => Op((maxDelay: java.lang.Long) => 
      ChangeNetwork(NetworkModels.withUniformRandomDelay(0, maxDelay, SimulationScenario.getRandom)));

  val delayOp = Op((_: Unit) => ChangeNetwork(NetworkModels.withDisconnectedModel(
                      new SocketIdExtractor(), NetworkModels.withUniformRandomDelay(0, maxLinkDelay, SimulationScenario.getRandom),
                      new java.util.HashSet(topology.slice(0, discNodes).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier]))));
  
  val startEPFDs = Op((self: Integer) => StartNode(AddressUtils.toAddress(self), EPFDParentInit(AddressUtils.toAddress(self)), 
        Map("epfd.simulation.topology" -> topology, "epfd.simulation.delay" -> delay, "epfd.simulation.delta" -> delta)));

  val killOP = Op{(self: Integer) => KillNode(AddressUtils.toAddress(self))}
  
  val scenario  =
      ValidationSimulation(prop) inParallel (
        raise(1, setupOp).arrival(Distributions.constant(0)), raise(1, setNormalLossyNetwork(), constant(0l)).arrival(constant(0)),
        raise(numNodes, startEPFDs, 1.toN).arrival(constant(0))) andThen 10000.millis afterTermination
        raise(1,delayOp).arrival(constant(0)) andThen 500.millis afterTermination
        raise(1, setNormalLossyNetwork(), constant(0)).arrival(constant(0)) andThen 10000.millis afterTermination
        raise(numNodes-discNodes, killOP, discNodes.toN).arrival(constant(0)) andThen 100000.millis afterTermination Terminate;
}

object EPFDTemplate extends App  {
      val seed = 123;
      JSimulationScenario.setSeed(seed);
      ValidationSimulation.runAndValidate(EPFDValidation(3, 2, 10, 5, 20).scenario);
}
