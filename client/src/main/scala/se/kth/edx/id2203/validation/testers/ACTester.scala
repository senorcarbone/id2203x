package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PP2P
import se.kth.edx.id2203.core.ExercisePrimitives.PP2P._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.kth.edx.id2203.validation.testers.TesterPrimitives.TesterPrimitives._
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.Timer
import se.sics.kompics.{KompicsEvent, Start}

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, _}
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

object ACTester {

  val LOG_STATE: String = "simulation.delivered"

  def apply[ARTYPE <: ComponentDefinition : TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(NNRegisterValidation[ARTYPE](2).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);


  class NNARInterceptor(init : Init[NNARInterceptor]) extends ProxyInterceptor[AtomicRegister]{

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    val nnar = requires[AtomicRegister];

    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
      }
    }

    nnar uponEvent {
      case x: AR_Read_Response => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[CRB_Deliver]]]);
        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[CRB_Deliver])
        //TODO
//        myList.add(x)
//        stateLogs.put(self, myList);
//        gv.setValue(LOG_STATE, stateLogs);
      }
      case x: AR_Write_Response => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[CRB_Deliver]]]);
        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[CRB_Deliver])
        //TODO
      }
    }

    override def getInterceptedPort(): PositivePort[AtomicRegister] = nnar

    override def getSelfAddress(): Address = self
  }
  
  class NNARParent(init: Init[NNARParent]) extends ComponentDefinition {

    val (selfAddr, topology, nnarClass, proxyInit) = init match {
      case Init(s: Address, t: Set[Address] @unchecked, nnarClass: Class[ComponentDefinition] @unchecked,
      m: ProxyInterceptorInit) => (s, t, nnarClass, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pp2p = create(classOf[PP2P], PP2PInit(selfAddr));
    val beb = create(classOf[ProvidedBroadcast], Init[ProvidedBroadcast](selfAddr, topology))
    val nnar = create(nnarClass, Init[ComponentDefinition](selfAddr, topology.size));
    val interceptor = create(classOf[NNARInterceptor], Init[NNARInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pp2p);
    connect[Timer](timer -> interceptor);

    connect[PerfectLink](pp2p -> beb);
    connect[PerfectLink](pp2p -> nnar);
    connect[BestEffortBroadcast](beb -> nnar);
    connect[AtomicRegister](nnar -> interceptor);
  }

  /**
    * (N,N) Atomic Register
    *
    * Properties Checked
    * ------------------ 
    * 
    *  NNAR1: Termination: If a correct process invokes an operation, then the operation eventually completes.
    *  NNAR2: Atomicity: Every read operation returns the value that was written most recently in a hypothetical execution, 
    *  where every failed operation appears to be complete or does not appear to have been invoked at all, 
    *  and every complete operation appears to have been executed at some instant between its invocation and its completion.
    *  
    */

  case class NNRegisterValidation[ARTYPE <: ComponentDefinition : TypeTag](numNodes: Int) {

    import Distributions._

    val msgs = Array(
//TODO
    );

    val topology = (1 to numNodes).map(toAddress(_)).toSet;

    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(LOG_STATE, HashMap.empty[Address, ListBuffer[Either[AR_Read_Response,AR_Write_Response]]])).build());

    val setNetworkDelay = () => Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));

    val startNNAR = Op((self: Integer) => StartNode(toAddress(self), Init[NNARParent](toAddress(self), topology,
      asJavaClass[ARTYPE](typeOf[ARTYPE]),
      ScheduleTriggers(toAddress(self))(
        //TODO
        (toAddress(1), 1, AR_Write_Request(Int.box(1))),
        (toAddress(2), 10, AR_Read_Request())
      ))));

    val prop1 =
      Property("Termination")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[Either[AR_Read_Response,AR_Write_Response]]]])
          .forall(x => true), 109999, //TODO
          "Every operation invoked by a correct process eventually completes",
          "There exist operations invoked by a correct process that never complete")).build();

    val prop2 =
      Property("Atomicity")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[Either[AR_Read_Response,AR_Write_Response]]]])
          .forall(x => true), 109999, //TODO
          "Every read operation returns the value that was written most recently in a hypothetical execution",
          "Not all read operations returns the value that was written most recently in a hypothetical execution")).build();

    //TODO
    val scenario =
      ValidationSimulation(prop1, prop2) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0l)), raise(1, setNetworkDelay(), constant(1l)).arrival(constant(0)),
        raise(numNodes, startNNAR, 1.toN).arrival(constant(0l))) andThen 1000000.millis afterTermination Terminate;
  }

}