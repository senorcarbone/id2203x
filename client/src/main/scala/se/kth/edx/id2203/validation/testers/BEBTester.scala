package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink.PerfectLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, PerfectP2PLink}
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

import scala.collection.JavaConverters._
import scala.collection.mutable._
import collection.immutable.Set
import collection.mutable.ListBuffer
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random

object BEBTester {

  val LOG_STATE = "simulation.delivered";

  def apply[BEB_TYPE <: ComponentDefinition: TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(BEBValidation[BEB_TYPE](10, 200).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);

  class BEBInterceptor(init: Init[BEBInterceptor]) extends ProxyInterceptor[BestEffortBroadcast] {

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    val beb = requires[BestEffortBroadcast];

    ctrl uponEvent {
      case _: Start => {
        initialize(pInit);
      }
    }

    beb uponEvent {
      case x: BEB_Deliver => {
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[BEB_Deliver]]]);
        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[BEB_Deliver])
        myList += (x)
        stateLogs.put(self, myList);
        gv.setValue(LOG_STATE, stateLogs);
      }
    }

    override def getInterceptedPort(): PositivePort[BestEffortBroadcast] = beb

    override def getSelfAddress(): Address = self
  }

  class BEBParent(init: Init[BEBParent]) extends ComponentDefinition {

    val (selfAddr, topology, javaClass, proxyInit) = init match {
      case Init(s: Address,
                t: Set[Address] @unchecked,
                j: Class[ComponentDefinition] @unchecked,
                m: ProxyInterceptorInit) =>
        (s, t, j, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pp2p = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val beb = create(javaClass, Init[ComponentDefinition](selfAddr, topology));
    val interceptor = create(classOf[BEBInterceptor], Init[BEBInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pp2p);
    connect[Timer](timer -> interceptor);

    connect[PerfectLink](pp2p -> beb);
    connect[BestEffortBroadcast](beb -> interceptor);
  }

  /**
    * Best Effort Broadcast Validation
    *
    * Properties Checked
    * ------------------
    * BEB1: Validity: If a correct process broadcasts a message m, then every correct process eventually delivers m.
    * BEB2: No duplication: No message is delivered more than once.
    * BEB3: No creation: If a process delivers a message m with sender s, then m was previously broadcast by process s.
    *
    */
  case class BEBValidation[BEB_TYPE <: ComponentDefinition: TypeTag](numNodes: Int, maxLinkDelay: Long) {

    import Distributions._

    val topology = (1 to numNodes).map(toAddress(_)).toSet;

    //Simulation Operations ----
    val setupOp = Op(
      (_: Unit) =>
        Setup((gv: GlobalView) => gv.setValue(LOG_STATE, HashMap.empty[Address, ListBuffer[BEB_Deliver]])).build()
    );

    val setNetwork = () =>
      Op(
        (maxDelay: java.lang.Long) =>
          ChangeNetwork(NetworkModels.withUniformRandomDelay(0, maxDelay, SimulationScenario.getRandom))
      );

    //random val for unique messages per experiment
    val simmsg = new Random().nextInt();

    val startBEB = Op(
      (self: Integer) =>
        StartNode(
          toAddress(self),
          Init[BEBParent](
            toAddress(self),
            topology,
            asJavaClass[BEB_TYPE](typeOf[BEB_TYPE]),
            ScheduleTriggers((9000L, BEB_Broadcast(new BroadcastedMessage(toAddress(self), simmsg))))
          )
        )
    );

    val prop1 =
      Property("No Duplication-No Creation")
        .addValidator(
          SimValidator(
            _.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[BEB_Deliver]]])
              .forall(_._2.forall {
                case x @ BEB_Deliver(src, y: BroadcastedMessage) => {
                  y.src == src && y.seq == simmsg;
                }
                case _ => {
                  false;
                }
              }),
            109999,
            "Every message delivered has been previously broadcasted",
            "Not all messages delivered are previously broadcasted"
          )
        )
        .addValidator(
          SimValidator(
            _.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[BEB_Deliver]]])
              .forall(x => x._2.size == x._2.distinct.size),
            109999,
            "No duplicate delivered messages found",
            "Duplicate deliveries found"
          )
        )
        .build();

    val prop2 =
      Property("Validity")
        .addValidator(
          SimValidator(
            gv => {
              val delivered = gv.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[BEB_Deliver]]]);
              topology.forall(node => {
                val msgs = delivered.get(node);
                if (!msgs.isDefined) {
                  false;
                } else {
                  val msgSet = msgs.get.foldLeft[Set[Address]](Set.empty[Address])((s, bebmsg) => s + bebmsg.src);
                  topology.subsetOf(msgSet)
                }
              })
            },
            109999,
            " Eventually every correct process delivers every message broadcasted by correct processes",
            "Not all messages broadcasted by correct processes are eventually delivered to all correct processes"
          )
        )
        .build();

    val scenario =
      ValidationSimulation(prop2, prop1) inParallel (raise(1, setupOp).arrival(Distributions.constant(0)), raise(
        1,
        setNetwork(),
        constant(0L)
      ).arrival(constant(0)),
      raise(numNodes, startBEB, 1.toN).arrival(constant(0))) andThen 1000000.millis afterTermination Terminate;

  }

}
