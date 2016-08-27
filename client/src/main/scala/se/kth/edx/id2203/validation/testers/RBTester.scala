package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink.PerfectLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, PerfectP2PLink, PerfectP2PLink$}
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.sics.kompics.{KompicsEvent, Start}
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.Timer

import scala.collection.JavaConversions._
import scala.collection.mutable._
import collection.immutable.Set
import collection.mutable.ListBuffer
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

object RBTester {

  val LOG_STATE: String = "simulation.delivered"

  def apply[BEBTYPE <: ComponentDefinition : TypeTag, RBTYPE <: ComponentDefinition : TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(RBValidation[BEBTYPE, RBTYPE](10, 4, 200).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);


  class RBInterceptor(init : Init[RBInterceptor]) extends ProxyInterceptor[ReliableBroadcast]{

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    val rb = requires[ReliableBroadcast];

    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
      }
    }

    rb uponEvent {
      case x: RB_Deliver => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[RB_Deliver]]]);
        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[RB_Deliver])
        myList.add(x)
        stateLogs.put(self, myList);
        gv.setValue(LOG_STATE, stateLogs);
      }
    }

    override def getInterceptedPort(): PositivePort[ReliableBroadcast] = rb

    override def getSelfAddress(): Address = self
  }

  class RBParent(init: Init[RBParent]) extends ComponentDefinition {

    val (selfAddr, topology, bebClass, rbClass, proxyInit) = init match {
      case Init(s: Address, t: Set[Address] @unchecked, bebc: Class[ComponentDefinition] @unchecked, 
      rbc: Class[ComponentDefinition] @unchecked, m: ProxyInterceptorInit) => (s, t, bebc, rbc, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pp2p = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val beb = create(bebClass, Init[ComponentDefinition](selfAddr, topology));
    val rb = create(rbClass, Init[ComponentDefinition](selfAddr));
    val interceptor = create(classOf[RBInterceptor], Init[RBInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pp2p);
    connect[Timer](timer -> interceptor);

    connect[PerfectLink](pp2p -> beb);
    connect[BestEffortBroadcast](beb -> rb);
    connect[ReliableBroadcast](rb -> interceptor);
  }

  /**
    * Regular Reliable Broadcast Validation
    *
    * Properties Checked
    * ------------------ 
    * RB1: Validity: If a correct process broadcasts a message m, then every correct process eventually delivers m.
    * RB2: No duplication: No message is delivered more than once.
    * RB3: No creation: If a process delivers a message m with sender s, then m was previously broadcast by process s.
    * RB4: Agreement: If a message m is delivered by some correct process, then m is eventually delivered by every correct process.
    */

  case class RBValidation[BEBTYPE <: ComponentDefinition : TypeTag, RBTYPE <: ComponentDefinition : TypeTag](numNodes: Int, discNodes: Int, maxLinkDelay: Long) {

    import Distributions._


    val topology = (1 to numNodes).map(toAddress(_)).toSet;
    val correct = (1 to discNodes).map(toAddress(_)).toSet;
    
    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(LOG_STATE, HashMap.empty[Address, ListBuffer[RB_Deliver]])).build());

    val setNetwork = () => Op((maxDelay: java.lang.Long) =>
      ChangeNetwork(NetworkModels.withUniformRandomDelay(0, maxDelay, SimulationScenario.getRandom)));

    val startRB = Op((self: Integer) => StartNode(toAddress(self), Init[RBParent](toAddress(self), topology, 
      asJavaClass[BEBTYPE](typeOf[BEBTYPE]), asJavaClass[RBTYPE](typeOf[RBTYPE]),
      ScheduleTriggers(
        (9000l, RB_Broadcast(new BroadcastedMessage(toAddress(self), 1)))
      ))));

    val killOP = Op { (self: Integer) => KillNode(toAddress(self)) }

    val prop1 =
      Property("No Duplication-No Creation")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[RB_Deliver]]])
          .forall(_._2.forall {
            case x@RB_Deliver(src, y: BroadcastedMessage) => {
              y.src == src && y.seq == 1;
            }
            case _ => {
              false;
            }
          }), 109999,
          "Every message delivered has been previously broadcasted",
          "Not all messages delivered are previously broadcasted"))
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[RB_Deliver]]])
          .forall(x => x._2.size == x._2.distinct.size), 109999,
          "No duplicate delivered messages found",
          "Duplicate deliveries found")).build();

    val prop2 =
      Property("Validity")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[RB_Deliver]]])
          .forall(_._2.foldLeft[Set[Address]](Set.empty[Address])((s,rbmsg) => s+rbmsg.src).containsAll(topology)), 109999,
          "Eventually every correct process delivers every message broadcasted by correct processes",
          "Not all messages broadcasted by correct processes are eventually delivered to all correct processes")).build();

    val prop3 =
      Property("Agreement")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[RB_Deliver]]])
          .filter(x => correct.contains(x._1)) //all correct nodes
          .groupBy(_._2.toSet).size == 1 // have delivered the same messages
          , 109999,
          "Every message delivered by a correct process is eventually delivered by every other correct process",
          "There exist messages delivered by a subset and not all of the correct processes")).build();

    val scenario =
      ValidationSimulation(prop2, prop1, prop3) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0)), raise(1, setNetwork(), constant(0l)).arrival(constant(0)),
        raise(numNodes, startRB, 1.toN).arrival(constant(0))) andThen 10000.millis afterTermination
        raise(numNodes - discNodes, killOP, discNodes.toN).arrival(constant(0)) andThen 100000.millis afterTermination Terminate;
  }

}

