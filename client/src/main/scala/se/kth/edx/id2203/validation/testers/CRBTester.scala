package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink.PerfectLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{PerfectP2PLink}
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
import scala.reflect.runtime.universe._
import scala.concurrent.duration._
import scala.util.Random

object CRBTester {

  val LOG_STATE: String = "simulation.delivered"

  def apply[BEBTYPE <: ComponentDefinition : TypeTag, RBTYPE <: ComponentDefinition : TypeTag, CRBTYPE <: ComponentDefinition : TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(CRBValidation[BEBTYPE, RBTYPE, CRBTYPE](2).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);


  class CRBInterceptor(init : Init[CRBInterceptor]) extends ProxyInterceptor[CausalOrderReliableBroadcast]{

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    val crb = requires[CausalOrderReliableBroadcast];

    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
      }
    }

    crb uponEvent {
      case x: CRB_Deliver => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[CRB_Deliver]]]);
        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[CRB_Deliver])
        myList.add(x)
        stateLogs.put(self, myList);
        gv.setValue(LOG_STATE, stateLogs);
      }
    }

    override def getInterceptedPort(): PositivePort[CausalOrderReliableBroadcast] = crb

    override def getSelfAddress(): Address = self
  }

  class CRBParent(init: Init[CRBParent]) extends ComponentDefinition {

    val (selfAddr, topology, bebClass, rbClass, crbClass, proxyInit) = init match {
      case Init(s: Address, t: Set[Address] @unchecked, bebc: Class[ComponentDefinition] @unchecked,
      rbc: Class[ComponentDefinition] @unchecked, crbc: Class[ComponentDefinition] @unchecked, 
      m: ProxyInterceptorInit) => (s, t, bebc, rbc, crbc, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pp2p = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val beb = create(bebClass, Init[ComponentDefinition](selfAddr, topology));
    val rb = create(rbClass, Init[ComponentDefinition](selfAddr));
    val crb = create(crbClass, Init[ComponentDefinition](selfAddr, topology));
    val interceptor = create(classOf[CRBInterceptor], Init[CRBInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pp2p);
    connect[Timer](timer -> interceptor);

    connect[PerfectLink](pp2p -> beb);
    connect[BestEffortBroadcast](beb -> rb);
    connect[ReliableBroadcast](rb -> crb);
    connect[CausalOrderReliableBroadcast](crb -> interceptor);
  }

  /**
    * Causal-Order Reliable Broadcast Validation
    *
    * Properties Checked
    * ------------------ 
    * CRB1: Validity: If a correct process broadcasts a message m, then every correct process eventually delivers m.
    * CRB2: No duplication: No message is delivered more than once.
    * CRB3: No creation: If a process delivers a message m with sender s, then m was previously broadcast by process s.
    * CRB4: Agreement: If a message m is delivered by some correct process, then m is eventually delivered by every correct process.
    * CRB5: Causal delivery: For any message m1 that potentially caused a message m2, i.e., m1 → m2, 
    *       no process delivers m2 unless it has already delivered m1.
    */

  case class CRBValidation[BEBTYPE <: ComponentDefinition : TypeTag, RBTYPE <: ComponentDefinition : TypeTag, CRBTYPE <: ComponentDefinition : TypeTag](numNodes: Int) {

    import Distributions._
    
    //random val for unique messages per experiment
    val rand = new Random();
    
    val msgs = Array(
      new BroadcastedMessage(toAddress(2), rand.nextInt()),
      new BroadcastedMessage(toAddress(1), rand.nextInt()),
      new BroadcastedMessage(toAddress(1), rand.nextInt())
    );
    
    val topology = (1 to numNodes).map(toAddress(_)).toSet;
    val correct = topology;
    
    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(LOG_STATE, HashMap.empty[Address, ListBuffer[CRB_Deliver]])).build());

    val setNetworkDelay = () => Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));

    val startCRB = Op((self: Integer) => StartNode(toAddress(self), Init[CRBParent](toAddress(self), topology,
      asJavaClass[BEBTYPE](typeOf[BEBTYPE]), asJavaClass[RBTYPE](typeOf[RBTYPE]),  asJavaClass[CRBTYPE](typeOf[CRBTYPE]),
      ScheduleTriggers(toAddress(self))(
        (toAddress(1), 100, CRB_Broadcast(msgs(1))), 
        (toAddress(1), 500, CRB_Broadcast(msgs(2))),
        (toAddress(2), 1400, CRB_Broadcast(msgs(0)))
      ))));

    val prop1 =
      Property("No Duplication-No Creation")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[CRB_Deliver]]])
          .forall(_._2.forall {
            case x@CRB_Deliver(src, y: BroadcastedMessage) => {
              msgs.contains(y)
            }
            case _ => {
              false;
            }
          }), 109999,
          "Every message delivered has been previously broadcasted",
          "Not all messages delivered are previously broadcasted"))
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[CRB_Deliver]]])
          .forall(x => x._2.size == x._2.distinct.size), 109999,
          "No duplicate delivered messages found",
          "Duplicate deliveries found")).build();

    val prop2 =
      Property("Validity")
        .addValidator(SimValidator(gv =>
        {
          val delivered = gv.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[CRB_Deliver]]]);
          topology.forall(node => {
            val del = delivered.get(node);
            if(!del.isDefined){
              false;
            }
            else{
              del.get.foldLeft[Set[BroadcastedMessage]](Set.empty[BroadcastedMessage])((s,bebmsg) => s+bebmsg.payload.asInstanceOf[BroadcastedMessage])
                .containsAll(msgs.toSet[BroadcastedMessage]);
            }
          })
        }, 109999,
          " Eventually every correct process delivers every message broadcasted by correct processes",
          "Not all messages broadcasted by correct processes are eventually delivered to all correct processes")).build();
    
    val prop3 =
      Property("Agreement")
        .addValidator(SimValidator(
          gv => {
            val delivered = gv.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[CRB_Deliver]]]);
            val someButSame = delivered.filter(x => correct.contains(x._1)) //all correct nodes
              .groupBy(_._2.toSet).size == 1 // have delivered the same messages
            val noneButSame = delivered.forall(_._2 == None);
            noneButSame || someButSame
          }
          , 109999,
          "Every message delivered by a correct process is eventually delivered by every other correct process",
          "There exist messages delivered by a subset and not all of the correct processes")).build();

    val prop4 =
      Property("Causal Delivery")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[CRB_Deliver]]])
          .forall(_._2.map(_.payload.asInstanceOf[BroadcastedMessage].seq).toList == msgs.map(_.seq).toList)
          , 109999,
          "All messages delivered respect causality order",
          "Some delivered messages violate causality guarantees. Make sure happened-before relations are maintained")).build();

    val scenario =
      ValidationSimulation(prop2, prop1, prop3, prop4) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0l)), raise(1, setNetworkDelay(), constant(1500l)).arrival(constant(0)),
        raise(numNodes, startCRB, 1.toN).arrival(constant(0l))) andThen 200.millis afterTermination
        raise(1, setNetworkDelay(), constant(100l)).arrival(constant(0)) andThen 1000000.millis afterTermination Terminate;    
  }

}

